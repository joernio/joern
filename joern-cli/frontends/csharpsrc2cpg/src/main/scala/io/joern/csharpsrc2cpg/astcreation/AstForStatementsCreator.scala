package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure, NewIdentifier}

import scala.::
import scala.util.{Success, Try}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForStatement(statement: ujson.Value): Seq[Ast] = {
    astForStatement(createDotNetNodeInfo(statement))
  }

  private def astForIfStatement(ifStmt: DotNetNodeInfo): Seq[Ast] = {
    val conditionNode = createDotNetNodeInfo(ifStmt.json(ParserKeys.Condition))
    val conditionAst  = astForNode(conditionNode).headOption.getOrElse(Ast())

    val thenNode     = createDotNetNodeInfo(ifStmt.json(ParserKeys.Statement))
    val thenAst: Ast = astForBlock(thenNode)
    val ifNode =
      controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if (${conditionNode.code})")
    val elseAst = ifStmt.json(ParserKeys.Else) match
      case elseStmt: ujson.Obj => astForElseStatement(createDotNetNodeInfo(elseStmt))
      case _                   => Ast()

    Seq(controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst)))
  }

  protected def astForStatement(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match {
      case ExpressionStatement => astForExpressionStatement(nodeInfo)
      case GlobalStatement     => astForGlobalStatement(nodeInfo)
      case IfStatement         => astForIfStatement(nodeInfo)
      case ThrowStatement      => astForThrowStatement(nodeInfo)
      case TryStatement        => astForTryStatement(nodeInfo)
      case ForEachStatement    => astForForEachStatement(nodeInfo)
      case ForStatement        => astForForStatement(nodeInfo)
      case DoStatement         => astForDoStatement(nodeInfo)
      case WhileStatement      => astForWhileStatement(nodeInfo)
      case SwitchStatement     => astForSwitchStatement(nodeInfo)
      case UsingStatement      => astForUsingStatement(nodeInfo)
      case _: JumpStatement    => astForJumpStatement(nodeInfo)
      case _                   => notHandledYet(nodeInfo)
    }
  }

  private def astForSwitchLabel(labelNode: DotNetNodeInfo): Seq[Ast] = {
    val caseNode = jumpTargetNode(labelNode, "case", labelNode.code)
    labelNode.node match
      case CasePatternSwitchLabel =>
        val patternNode = createDotNetNodeInfo(labelNode.json(ParserKeys.Pattern)(ParserKeys.Expression))
        Ast(caseNode) +: astForNode(patternNode)
      case CaseSwitchLabel =>
        val valueNode = createDotNetNodeInfo(labelNode.json(ParserKeys.Value))
        Ast(caseNode) +: astForNode(valueNode)
      case DefaultSwitchLabel => Seq(Ast(caseNode))
      case _                  => Seq(Ast())
  }

  private def astForSwitchStatement(switchStmt: DotNetNodeInfo): Seq[Ast] = {
    val comparatorNode    = createDotNetNodeInfo(switchStmt.json(ParserKeys.Expression))
    val comparatorNodeAst = astForExpression(comparatorNode).headOption

    val switchBodyAsts: Seq[Ast] = switchStmt
      .json(ParserKeys.Sections)
      .arr
      .flatMap(section =>
        val sectionNode = section match
          case value: ujson.Obj   => createDotNetNodeInfo(value)
          case value: ujson.Value => nullSafeCreateParserNodeInfo(Option(value))

        val labelNodes = sectionNode.json(ParserKeys.Labels).arr
        labelNodes.flatMap(labelNode => astForSwitchLabel(createDotNetNodeInfo(labelNode))) :+ astForBlock(sectionNode)
      )
      .toSeq

    val switchNode = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, s"switch (${comparatorNode.code})");

    Seq(controlStructureAst(switchNode, comparatorNodeAst, switchBodyAsts))
  }

  private def astForWhileStatement(whileStmt: DotNetNodeInfo): Seq[Ast] = {
    val whileBlock    = createDotNetNodeInfo(whileStmt.json(ParserKeys.Statement))
    val whileBlockAst = astForBlock(whileBlock)

    val condition    = createDotNetNodeInfo(whileStmt.json(ParserKeys.Condition))
    val conditionAst = astForNode(condition)

    val code = s"while (${condition.code})"

    val whileNode = controlStructureNode(whileStmt, ControlStructureTypes.WHILE, code)

    Seq(Ast(whileNode).withChild(whileBlockAst).withChildren(conditionAst))
  }

  private def astForDoStatement(doStmt: DotNetNodeInfo): Seq[Ast] = {
    val doBlock    = createDotNetNodeInfo(doStmt.json(ParserKeys.Statement))
    val doBlockAst = astForBlock(doBlock)

    val condition    = createDotNetNodeInfo(doStmt.json(ParserKeys.Condition))
    val conditionAst = astForNode(condition)

    val code        = s"do {...} while (${condition.code})"
    val doBlockNode = controlStructureNode(doStmt, ControlStructureTypes.DO, code)

    Seq(Ast(doBlockNode).withChild(doBlockAst).withChildren(conditionAst))
  }

  private def astForForStatement(forStmt: DotNetNodeInfo): Seq[Ast] = {
    val initNode        = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Declaration))
    val conditionNode   = nullSafeCreateParserNodeInfo(forStmt.json.obj.get(ParserKeys.Condition))
    val incrementorNode = nullSafeCreateParserNodeInfo(forStmt.json(ParserKeys.Incrementors).arr.headOption)

    val forBodyAst = astForBlock(createDotNetNodeInfo(forStmt.json(ParserKeys.Statement)))

    val code = s"for (${initNode.code};${conditionNode.code};${incrementorNode.code})"
    val forNode =
      controlStructureNode(forStmt, ControlStructureTypes.FOR, code);

    val initNodeAst    = astForNode(initNode)
    val conditionAst   = astForNode(conditionNode)
    val incrementorAst = astForNode(incrementorNode)

    val _forAst = Ast(forNode)
      .withChildren(initNodeAst)
      .withChildren(conditionAst)
      .withChildren(incrementorAst)
      .withChild(forBodyAst)
      .withConditionEdges(forNode, conditionAst.flatMap(_.root).toList)

    Seq(_forAst)
  }

  private def astForForEachStatement(forEachStmt: DotNetNodeInfo): Seq[Ast] = {
    val forEachNode     = controlStructureNode(forEachStmt, ControlStructureTypes.FOR, forEachStmt.code)
    val iterableAst     = astForNode(forEachStmt.json(ParserKeys.Expression))
    val forEachBlockAst = astForBlock(createDotNetNodeInfo(forEachStmt.json(ParserKeys.Statement)))

    val identifierValue = forEachStmt.json(ParserKeys.Identifier)(ParserKeys.Value).str
    val _identifierNode =
      identifierNode(
        node = createDotNetNodeInfo(forEachStmt.json(ParserKeys.Type)),
        name = identifierValue,
        code = identifierValue,
        typeFullName = nodeTypeFullName(createDotNetNodeInfo(forEachStmt.json(ParserKeys.Type)))
      )

    val iteratorVarAst = Ast(_identifierNode)

    Seq(Ast(forEachNode).withChild(iteratorVarAst).withChildren(iterableAst).withChild(forEachBlockAst))
  }

  private def astForElseStatement(elseParserNode: DotNetNodeInfo): Ast = {
    val elseNode = controlStructureNode(elseParserNode, ControlStructureTypes.ELSE, "else")

    Option(elseParserNode.json(ParserKeys.Statement)) match
      case Some(elseStmt: ujson.Value) if createDotNetNodeInfo(elseStmt).node == Block =>
        val blockAst: Ast = astForBlock(createDotNetNodeInfo(elseParserNode.json(ParserKeys.Statement)))
        Ast(elseNode).withChild(blockAst)
      case Some(elseStmt) =>
        astForNode(createDotNetNodeInfo(elseParserNode.json(ParserKeys.Statement))).headOption.getOrElse(Ast())
      case None => Ast()

  }

  protected def astForGlobalStatement(globalStatement: DotNetNodeInfo): Seq[Ast] = {
    astForNode(globalStatement.json(ParserKeys.Statement))
  }

  private def astForJumpStatement(jumpStmt: DotNetNodeInfo): Seq[Ast] = {
    jumpStmt.node match
      case BreakStatement    => Seq(Ast(controlStructureNode(jumpStmt, ControlStructureTypes.BREAK, jumpStmt.code)))
      case ContinueStatement => Seq(Ast(controlStructureNode(jumpStmt, ControlStructureTypes.CONTINUE, jumpStmt.code)))
      case GotoStatement     => astForGotoStatement(jumpStmt)
      case ReturnStatement   => astForReturnStatement(jumpStmt)
      case _                 => Seq.empty
  }

  private def astForGotoStatement(gotoStmt: DotNetNodeInfo): Seq[Ast] = {
    val identifierAst = Option(gotoStmt.json(ParserKeys.Expression)) match
      case Some(value: ujson.Obj) => astForNode(createDotNetNodeInfo(value))
      case _                      => Seq.empty

    val gotoAst = Ast(controlStructureNode(gotoStmt, ControlStructureTypes.GOTO, gotoStmt.code))

    identifierAst :+ gotoAst
  }

  private def astForReturnStatement(returnStmt: DotNetNodeInfo): Seq[Ast] = {
    val identifierAst = Option(returnStmt.json(ParserKeys.Expression)) match {
      case Some(value: ujson.Obj) => astForNode(createDotNetNodeInfo(value))
      case _                      => Seq.empty
    }
    val _returnNode = returnNode(returnStmt, returnStmt.code)
    Seq(returnAst(_returnNode, identifierAst))
  }

  protected def astForThrowStatement(throwStmt: DotNetNodeInfo): Seq[Ast] = {
    val expr = createDotNetNodeInfo(throwStmt.json(ParserKeys.Expression))
    val args = astForNode(expr)
    val throwCall = createCallNodeForOperator(
      throwStmt,
      CSharpOperators.throws,
      typeFullName = Option(getTypeFullNameFromAstNode(args))
    )
    Seq(callAst(throwCall, args))
  }

  protected def astForTryStatement(tryStmt: DotNetNodeInfo): Seq[Ast] = {
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("try")
      .lineNumber(line(tryStmt))
      .columnNumber(column(tryStmt))
    val tryAst = astForBlock(createDotNetNodeInfo(tryStmt.json(ParserKeys.Block)), Option("try"))

    val catchAsts = Try(tryStmt.json(ParserKeys.Catches)).map(_.arr.flatMap(astForNode).toSeq).getOrElse(Seq.empty)

    val finallyBlock = Try(createDotNetNodeInfo(tryStmt.json(ParserKeys.Finally))).map(astForFinallyClause) match
      case Success(finallyAst :: Nil) =>
        finallyAst.root.collect { case x: NewBlock => x.code("finally") }
        finallyAst
      case _ => Ast()

    val controlStructureAst = tryCatchAst(tryNode, tryAst, catchAsts, Option(finallyBlock))
    Seq(controlStructureAst)
  }

  protected def astForFinallyClause(finallyClause: DotNetNodeInfo): Seq[Ast] = {
    Seq(astForBlock(createDotNetNodeInfo(finallyClause.json(ParserKeys.Block)), code = Option(code(finallyClause))))
  }

  /** Variables using the <a
    * href="https://learn.microsoft.com/en-us/dotnet/api/system.idisposable?view=net-8.0">IDisposable</a> interface may
    * be used in `using`, where a call to `Dispose` is guaranteed.
    *
    * Thus, this is lowered as a try-finally, with finally making a call to `Dispose` on the declared variable.
    */
  private def astForUsingStatement(usingStmt: DotNetNodeInfo): Seq[Ast] = {
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .code("try")
      .lineNumber(line(usingStmt))
      .columnNumber(column(usingStmt))
    val tryAst   = astForBlock(createDotNetNodeInfo(usingStmt.json(ParserKeys.Statement)), Option("try"))
    val declNode = createDotNetNodeInfo(usingStmt.json(ParserKeys.Declaration))
    val declAst  = astForNode(declNode)

    val finallyAst = declAst.flatMap(_.nodes).collectFirst { case x: NewIdentifier => x.copy }.map { id =>
      val callCode = s"${id.name}.Dispose()"
      id.code(callCode)
      val disposeCall = callNode(
        usingStmt,
        callCode,
        "Dispose",
        "System.Disposable.Dispose:System.Void()",
        DispatchTypes.DYNAMIC_DISPATCH,
        Option("System.Void()"),
        Option("System.Void")
      )
      val disposeAst = callAst(disposeCall, receiver = Option(Ast(id)))
      Ast(blockNode(usingStmt).code("finally"))
        .withChild(disposeAst)
    }

    declAst :+ tryCatchAst(tryNode, tryAst, Seq.empty, finallyAst)
  }

  protected def astForCatchClause(catchClause: DotNetNodeInfo): Seq[Ast] = {
    val declAst = astForNode(catchClause.json(ParserKeys.Declaration)).toList
    val blockAst = astForBlock(
      createDotNetNodeInfo(catchClause.json(ParserKeys.Block)),
      code = Option(code(catchClause)),
      prefixAsts = declAst
    )
    Seq(blockAst)
  }

  protected def astForCatchDeclaration(catchDeclaration: DotNetNodeInfo): Seq[Ast] = {
    val name         = nameFromNode(catchDeclaration)
    val typeFullName = nodeTypeFullName(catchDeclaration)
    val _localNode   = localNode(catchDeclaration, name, name, typeFullName)
    val localNodeAst = Ast(_localNode)
    scope.addToScope(name, _localNode)
    Seq(localNodeAst)
  }

}
