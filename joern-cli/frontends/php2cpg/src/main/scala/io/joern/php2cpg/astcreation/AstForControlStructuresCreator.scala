package io.joern.php2cpg.astcreation

import io.joern.php2cpg.utils.PhpScopeElement
import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain.*
import io.joern.x2cpg.Defines.UnresolvedSignature
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.proto.cpg.Cpg.CpgStruct.Edge.EdgeType

trait AstForControlStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForBreakStmt(breakStmt: PhpBreakStmt): Ast = {
    val code      = breakStmt.num.map(num => s"break($num)").getOrElse("break")
    val breakNode = controlStructureNode(breakStmt, ControlStructureTypes.BREAK, code)

    val argument = breakStmt.num.map(intToLiteralAst)

    controlStructureAst(breakNode, None, argument.toList)
  }

  protected def astForContinueStmt(continueStmt: PhpContinueStmt): Ast = {
    val code         = continueStmt.num.map(num => s"continue($num)").getOrElse("continue")
    val continueNode = controlStructureNode(continueStmt, ControlStructureTypes.CONTINUE, code)

    val argument = continueStmt.num.map(intToLiteralAst)

    controlStructureAst(continueNode, None, argument.toList)
  }

  protected def astForWhileStmt(whileStmt: PhpWhileStmt): Ast = {
    val condition  = astForExpr(whileStmt.cond)
    val lineNumber = line(whileStmt)
    val code       = s"while (${condition.rootCodeOrEmpty})"
    val body       = stmtBodyBlockAst(whileStmt)

    whileAst(Option(condition), List(body), Option(code), lineNumber)
  }

  protected def astForDoStmt(doStmt: PhpDoStmt): Ast = {
    val condition  = astForExpr(doStmt.cond)
    val lineNumber = line(doStmt)
    val code       = s"do {...} while (${condition.rootCodeOrEmpty})"
    val body       = stmtBodyBlockAst(doStmt)

    doWhileAst(Option(condition), List(body), Option(code), lineNumber)
  }

  protected def astForForStmt(stmt: PhpForStmt): Ast = {
    val initAsts      = stmt.inits.map(astForExpr)
    val conditionAsts = stmt.conditions.map(astForExpr)
    val loopExprAsts  = stmt.loopExprs.map(astForExpr)

    val bodyAst = stmtBodyBlockAst(stmt)

    val initCode      = initAsts.map(_.rootCodeOrEmpty).mkString(",")
    val conditionCode = conditionAsts.map(_.rootCodeOrEmpty).mkString(",")
    val loopExprCode  = loopExprAsts.map(_.rootCodeOrEmpty).mkString(",")
    val forCode       = s"for ($initCode;$conditionCode;$loopExprCode)"

    val forNode = controlStructureNode(stmt, ControlStructureTypes.FOR, forCode)
    forAst(forNode, Nil, initAsts, conditionAsts, loopExprAsts, bodyAst)
  }

  protected def astForIfStmt(ifStmt: PhpIfStmt): Ast = {
    val condition = astForExpr(ifStmt.cond)

    val thenAst = stmtBodyBlockAst(ifStmt)

    val elseAst = ifStmt.elseIfs match {
      case Nil => ifStmt.elseStmt.map(els => stmtBodyBlockAst(els)).toList

      case elseIf :: rest =>
        val newIfStmt     = PhpIfStmt(elseIf.cond, elseIf.stmts, rest, ifStmt.elseStmt, elseIf.attributes)
        val wrappingBlock = blockNode(elseIf)
        val wrappedAst    = Ast(wrappingBlock).withChild(astForIfStmt(newIfStmt)) :: Nil
        wrappedAst
    }

    val conditionCode = condition.rootCodeOrEmpty
    val ifNode        = controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if ($conditionCode)")

    controlStructureAst(ifNode, Option(condition), thenAst :: elseAst)
  }

  protected def astForSwitchStmt(stmt: PhpSwitchStmt): Ast = {
    val conditionAst = astForExpr(stmt.condition)

    val switchNode =
      controlStructureNode(stmt, ControlStructureTypes.SWITCH, s"switch (${conditionAst.rootCodeOrEmpty})")

    val switchBodyBlock = blockNode(stmt)
    val entryAsts       = stmt.cases.flatMap(astsForSwitchCase)
    val switchBody      = Ast(switchBodyBlock).withChildren(entryAsts)

    controlStructureAst(switchNode, Option(conditionAst), switchBody :: Nil)
  }

  private def astsForSwitchCase(caseStmt: PhpCaseStmt): List[Ast] = {
    val maybeConditionAst = caseStmt.condition.map(astForExpr)
    val jumpTarget = maybeConditionAst match {
      case Some(conditionAst) => NewJumpTarget().name("case").code(s"case ${conditionAst.rootCodeOrEmpty}")
      case None               => NewJumpTarget().name("default").code("default")
    }
    jumpTarget.lineNumber(line(caseStmt))

    val stmtAsts = caseStmt.stmts.flatMap(astsForStmt)

    Ast(jumpTarget) :: maybeConditionAst.toList ++ stmtAsts
  }

  protected def astForTryStmt(stmt: PhpTryStmt): Ast = {
    val tryBody = stmtBodyBlockAst(stmt)

    scope.pushNewScope(NewBlock())
    val catches = stmt.catches.map { catchStmt =>

      val localCatchVariable = catchStmt.variable
        .collectFirst { case variable @ PhpVariable(name: PhpNameExpr, _) =>
          val local           = localNode(variable, name.name, name.name, Defines.Any)
          val phpScopeElement = scope.addToScope(name.name, local)
          diffGraph.addEdge(phpScopeElement.node, local, EdgeTypes.AST)
          local.dynamicTypeHintFullName(catchStmt.types.map(_.name))
          Ast(local)
        }
        .getOrElse(Ast())

      val variableName = localCatchVariable.rootCode match {
        case Some(code) if code != "" =>
          s" $$$code"
        case _ => ""
      }

      val catchNodeCode = s"catch (${catchStmt.types.map(_.name).mkString(" | ")}$variableName)";
      val catchNode     = controlStructureNode(catchStmt, ControlStructureTypes.CATCH, catchNodeCode)

      Ast(catchNode).withChild(astForCatchStmt(catchStmt)).withChild(localCatchVariable)
    }
    scope.popScope()

    val finallyBody = stmt.finallyStmt.map { fin =>
      val finallyNode = controlStructureNode(fin, ControlStructureTypes.FINALLY, "finally")
      Ast(finallyNode).withChild(stmtBodyBlockAst(fin))
    }

    val tryNode = controlStructureNode(stmt, ControlStructureTypes.TRY, "try { ... }")
    tryCatchAst(tryNode, tryBody, catches, finallyBody)
  }

  private def astForCatchStmt(stmt: PhpCatchStmt): Ast = {
    stmtBodyBlockAst(stmt)
  }

  protected def astForReturnStmt(stmt: PhpReturnStmt): Ast = {
    val maybeExprAst = stmt.expr.map(astForExpr)
    val code         = s"return ${maybeExprAst.map(_.rootCodeOrEmpty).getOrElse("")}"

    val node = returnNode(stmt, code)

    returnAst(node, maybeExprAst.toList)
  }

  protected def astForForeachStmt(stmt: PhpForeachStmt): Ast = {
    val iterIdentifier = getTmpIdentifier(stmt, maybeTypeFullName = None, prefix = "iter_")
    val localN         = handleVariableOccurrence(stmt, iterIdentifier.name)

    // keep this just used to construct the `code` field
    val assignItemTargetAst = stmt.keyVar match {
      case Some(key) => astForKeyValPair(stmt, key, stmt.valueVar)
      case None      => astForExpr(stmt.valueVar)
    }

    // Initializer asts
    // - Iterator assign
    val iterValue         = astForExpr(stmt.iterExpr)
    val iteratorAssignAst = simpleAssignAst(stmt, Ast(iterIdentifier).withRefEdge(iterIdentifier, localN), iterValue)

    // - Assigned item assign
    val itemInitAst = getItemAssignAstForForeach(stmt, iterIdentifier.copy)

    // Condition ast
    val isNullName = PhpOperators.isNull
    val valueAst   = astForExpr(stmt.valueVar)
    val isNullCode = s"$isNullName(${valueAst.rootCodeOrEmpty})"
    val isNullCall = operatorCallNode(stmt, isNullCode, isNullName, Some(TypeConstants.Bool))
      .methodFullName(PhpOperators.isNull)
    val notIsNull    = operatorCallNode(stmt, s"!$isNullCode", Operators.logicalNot, None)
    val isNullAst    = callAst(isNullCall, valueAst :: Nil)
    val conditionAst = callAst(notIsNull, isNullAst :: Nil)

    // Update asts
    val nextIterIdentCopy = iterIdentifier.copy
    val nextIterIdent     = Ast(nextIterIdentCopy).withRefEdge(nextIterIdentCopy, localN)
    val nextSignature     = "void()"
    val nextCallCode      = s"${nextIterIdent.rootCodeOrEmpty}${InstanceMethodDelimiter}next()"
    val nextCallNode = callNode(
      stmt,
      nextCallCode,
      "next",
      "Iterator.next",
      DispatchTypes.DYNAMIC_DISPATCH,
      Some(nextSignature),
      Some(Defines.Any)
    )
    val nextCallAst = callAst(nextCallNode, base = Option(nextIterIdent))
    val itemUpdateAst = itemInitAst.root match {
      case Some(initRoot: AstNodeNew) => itemInitAst.subTreeCopy(initRoot)
      case _ =>
        logger.warn(s"Could not copy foreach init ast in $relativeFileName")
        Ast()
    }

    val bodyAst = stmtBodyBlockAst(stmt)

    val ampPrefix   = if (stmt.assignByRef) "&" else ""
    val foreachCode = s"foreach (${iterValue.rootCodeOrEmpty} as $ampPrefix${assignItemTargetAst.rootCodeOrEmpty})"
    val foreachNode = controlStructureNode(stmt, ControlStructureTypes.FOR, foreachCode)
    Ast(foreachNode)
      .withChild(wrapMultipleInBlock(iteratorAssignAst :: itemInitAst :: Nil, line(stmt)))
      .withChild(conditionAst)
      .withChild(wrapMultipleInBlock(nextCallAst :: itemUpdateAst :: Nil, line(stmt)))
      .withChild(bodyAst)
      .withConditionEdges(foreachNode, conditionAst.root.toList)
  }

  private def getItemAssignAstForForeach(stmt: PhpForeachStmt, iteratorIdentifier: NewIdentifier): Ast = {
    val block = blockNode(stmt)
    scope.pushNewScope(block)

    val localN = handleVariableOccurrence(stmt, iteratorIdentifier.name)
    // create assignment for value-part
    val valueAssign = {
      val iteratorIdentifierAst = Ast(iteratorIdentifier).withRefEdge(iteratorIdentifier, localN)
      val currentCallSignature  = s"$UnresolvedSignature(0)"
      val currentCallCode       = s"${iteratorIdentifierAst.rootCodeOrEmpty}${InstanceMethodDelimiter}current()"
      // `current` function is used to get the current element of given array
      // see https://www.php.net/manual/en/function.current.php & https://www.php.net/manual/en/iterator.current.php
      val currentCallNode = callNode(
        stmt,
        currentCallCode,
        "current",
        "Iterator.current",
        DispatchTypes.DYNAMIC_DISPATCH,
        Some(currentCallSignature),
        Some(Defines.Any)
      )
      val currentCallAst = callAst(currentCallNode, base = Option(iteratorIdentifierAst))

      val valueAst = if (stmt.assignByRef) {
        val addressOfCode = s"&${currentCallAst.rootCodeOrEmpty}"
        val addressOfCall = operatorCallNode(stmt, addressOfCode, Operators.addressOf, None)
        callAst(addressOfCall, currentCallAst :: Nil)
      } else {
        currentCallAst
      }
      simpleAssignAst(stmt, astForExpr(stmt.valueVar), valueAst)
    }

    // try to create assignment for key-part
    val keyAssignOption = stmt.keyVar.map(keyVar =>
      val iteratorIdentifierCopyKey = iteratorIdentifier.copy
      val iteratorIdentifierAst     = Ast(iteratorIdentifierCopyKey).withRefEdge(iteratorIdentifierCopyKey, localN)
      val keyCallSignature          = s"$UnresolvedSignature(0)"
      val keyCallCode               = s"${iteratorIdentifierAst.rootCodeOrEmpty}${InstanceMethodDelimiter}key()"
      // `key` function is used to get the key of the current element
      // see https://www.php.net/manual/en/function.key.php & https://www.php.net/manual/en/iterator.key.php
      val keyCallNode = callNode(
        stmt,
        keyCallCode,
        "key",
        "Iterator.key",
        DispatchTypes.DYNAMIC_DISPATCH,
        Some(keyCallSignature),
        Some(Defines.Any)
      )
      val keyCallAst = callAst(keyCallNode, base = Option(iteratorIdentifierAst))
      simpleAssignAst(stmt, astForExpr(keyVar), keyCallAst)
    )

    scope.popScope()

    Ast(block).withChildren(keyAssignOption.toList :+ valueAssign)
  }

  protected def astForThrow(expr: PhpThrowExpr): Ast = {
    val thrownExpr = astForExpr(expr.expr)
    val code       = s"throw ${thrownExpr.rootCodeOrEmpty}"

    val throwNode = controlStructureNode(expr, ControlStructureTypes.THROW, code)

    Ast(throwNode).withChild(thrownExpr)
  }

  protected def astForYieldFromExpr(expr: PhpYieldFromExpr): Ast = {
    // TODO This is currently only distinguishable from yield by the code field. Decide whether to treat YIELD_FROM
    //  separately or whether to lower this to a foreach with regular yields.
    val exprAst = astForExpr(expr.expr)

    val code = s"yield from ${exprAst.rootCodeOrEmpty}"

    val yieldNode = controlStructureNode(expr, ControlStructureTypes.YIELD, code)

    Ast(yieldNode)
      .withChild(exprAst)
  }

  protected def astForGotoStmt(stmt: PhpGotoStmt): Ast = {
    val label = stmt.label.name
    val code  = s"goto $label"

    val gotoNode = controlStructureNode(stmt, ControlStructureTypes.GOTO, code)

    val jumpLabel = NewJumpLabel()
      .name(label)
      .code(label)
      .lineNumber(line(stmt))

    controlStructureAst(gotoNode, condition = None, children = Ast(jumpLabel) :: Nil)
  }

  protected def astForLabelStmt(stmt: PhpLabelStmt): Ast = {
    val label = stmt.label.name

    val jumpTarget = NewJumpTarget()
      .name(label)
      .code(label)
      .lineNumber(line(stmt))

    Ast(jumpTarget)
  }

}
