package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.nodes.{NewFieldIdentifier, NewIdentifier, NewLiteral, NewLocal}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, ModifierTypes, Operators}

import scala.::
import scala.util.{Try, Success, Failure}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForStatement(statement: ujson.Value): Seq[Ast] = {
    astForStatement(createDotNetNodeInfo(statement))
  }

  /** Separates the `AST` result of a conditional expression into the condition as well as any declared variables to
    * prepend.
    * @param conditionAst
    *   the condition.
    * @param prependIfBody
    *   statements to prepend to the `if`/`then` body.
    */
  final case class ConditionAstResult(conditionAst: Ast, prependIfBody: List[Ast])

  // TODO: Use this method elsewhere on other control structures
  protected def astForConditionNode(condNode: DotNetNodeInfo): ConditionAstResult = {
    lazy val default = ConditionAstResult(astForNode(condNode).headOption.getOrElse(Ast()), List.empty)
    condNode.node match {
      case x: PatternExpr =>
        astsForIsPatternExpression(condNode) match {
          case head :: tail => ConditionAstResult(head, tail)
          case Nil =>
            logger.warn(
              s"Unable to handle pattern expression $x in condition expression, resorting to default behaviour"
            )
            default
        }
      case _ => default
    }
  }

  private def astForIfStatement(ifStmt: DotNetNodeInfo): Seq[Ast] = {
    val conditionNode                                   = createDotNetNodeInfo(ifStmt.json(ParserKeys.Condition))
    val ConditionAstResult(conditionAst, prependIfBody) = astForConditionNode(conditionNode)

    val thenNode     = createDotNetNodeInfo(ifStmt.json(ParserKeys.Statement))
    val thenAst: Ast = astForBlock(thenNode, prefixAsts = prependIfBody)
    val ifNode =
      controlStructureNode(ifStmt, ControlStructureTypes.IF, s"if (${conditionNode.code})")
    val elseAst = ifStmt.json(ParserKeys.Else) match
      case elseStmt: ujson.Obj => astForElseStatement(createDotNetNodeInfo(elseStmt))
      case _                   => Ast()

    Seq(controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst)))
  }

  protected def astForStatement(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match {
      case ExpressionStatement    => astForExpressionStatement(nodeInfo)
      case GlobalStatement        => astForGlobalStatement(nodeInfo)
      case IfStatement            => astForIfStatement(nodeInfo)
      case ThrowStatement         => astForThrowStatement(nodeInfo)
      case TryStatement           => astForTryStatement(nodeInfo)
      case ForEachStatement       => astForForEachStatement(nodeInfo)
      case ForStatement           => astForForStatement(nodeInfo)
      case DoStatement            => astForDoStatement(nodeInfo)
      case WhileStatement         => astForWhileStatement(nodeInfo)
      case SwitchStatement        => astForSwitchStatement(nodeInfo)
      case UsingStatement         => astForUsingStatement(nodeInfo)
      case LocalFunctionStatement => astForLocalFunctionStatement(nodeInfo)
      case _: JumpStatement       => astForJumpStatement(nodeInfo)
      case _                      => notHandledYet(nodeInfo)
    }
  }

  private def astForLocalFunctionStatement(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    astForMethodDeclaration(nodeInfo)
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

    val switchNode = controlStructureNode(switchStmt, ControlStructureTypes.SWITCH, s"switch (${comparatorNode.code})")
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
    val int32Tfn    = BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
    val forEachNode = controlStructureNode(forEachStmt, ControlStructureTypes.FOR, forEachStmt.code)
    // Create the collection AST
    def newCollectionAst = astForNode(forEachStmt.json(ParserKeys.Expression))
    val collectionNode   = createDotNetNodeInfo(forEachStmt.json(ParserKeys.Expression))
    val collectionCode   = code(collectionNode)
    // Create the iterator variable
    val iterName    = forEachStmt.json(ParserKeys.Identifier)(ParserKeys.Value).str
    val iterNode    = forEachStmt.json(ParserKeys.Type)
    val iterNodeTfn = nodeTypeFullName(createDotNetNodeInfo(iterNode))
    val iterIdentifier =
      identifierNode(
        node = createDotNetNodeInfo(iterNode),
        name = iterName,
        code = iterName,
        typeFullName = iterNodeTfn
      )
    val iterVarLocal = NewLocal().name(iterName).code(iterName).typeFullName(iterNodeTfn)
    scope.addToScope(iterName, iterVarLocal)
    // Create a de-sugared `idx` variable, i.e., var _idx_ = 0
    val idxName         = "_idx_"
    val idxLocal        = NewLocal().name(idxName).code(idxName).typeFullName(int32Tfn)
    val idxIdenAtAssign = identifierNode(node = collectionNode, name = idxName, code = idxName, typeFullName = int32Tfn)
    val idxAssignment =
      callNode(forEachStmt, s"$idxName = 0", Operators.assignment, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
    val idxAssigmentArgs =
      List(Ast(idxIdenAtAssign), Ast(NewLiteral().code("0").typeFullName(BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int))))
    val idxAssignmentAst = callAst(idxAssignment, idxAssigmentArgs)
    // Create condition based on `idx` variable, i.e., _idx_ < $collection.Count
    val idxIdAtCond = idxIdenAtAssign.copy
    val collectCountAccess = callNode(
      forEachStmt,
      s"$collectionCode.Count",
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH
    )
    val fieldAccessAst =
      callAst(collectCountAccess, newCollectionAst :+ Ast(NewFieldIdentifier().canonicalName("Count").code("Count")))
    val idxLt =
      callNode(
        forEachStmt,
        s"$idxName < $collectionCode.Count",
        Operators.lessThan,
        Operators.lessThan,
        DispatchTypes.STATIC_DISPATCH
      )
    val idxLtArgs =
      List(Ast(idxIdAtCond), fieldAccessAst)
    val ltCallCond = callAst(idxLt, idxLtArgs)
    // Create the assignment from $element = $collection[_idx_++]
    val idxIdAtCollAccess = idxIdenAtAssign.copy
    val collectIdxAccess = callNode(
      forEachStmt,
      s"$collectionCode[$idxName++]",
      Operators.indexAccess,
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH
    )
    val postIncrAst = callAst(
      callNode(
        forEachStmt,
        s"$idxName++",
        Operators.postIncrement,
        Operators.postIncrement,
        DispatchTypes.STATIC_DISPATCH
      ),
      Ast(idxIdAtCollAccess) :: Nil
    )
    val indexAccessAst = callAst(collectIdxAccess, newCollectionAst :+ postIncrAst)
    val iteratorAssignmentNode =
      callNode(
        forEachStmt,
        s"$iterName = $collectionCode[$idxName++]",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
    val iteratorAssignmentArgs = List(Ast(iterIdentifier), indexAccessAst)
    val iteratorAssignmentAst  = callAst(iteratorAssignmentNode, iteratorAssignmentArgs)

    val forEachBlockAst = astForBlock(createDotNetNodeInfo(forEachStmt.json(ParserKeys.Statement)))

    forAst(
      forNode = forEachNode,
      locals = Ast(idxLocal)
        .withRefEdge(idxIdenAtAssign, idxLocal)
        .withRefEdge(idxIdAtCond, idxLocal)
        .withRefEdge(idxIdAtCollAccess, idxLocal) :: Ast(iterVarLocal).withRefEdge(iterIdentifier, iterVarLocal) :: Nil,
      conditionAsts = ltCallCond :: Nil,
      initAsts = idxAssignmentAst :: Nil,
      updateAsts = iteratorAssignmentAst :: Nil,
      bodyAst = forEachBlockAst
    ) :: Nil
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

  private def astForGlobalStatement(globalStatement: DotNetNodeInfo): Seq[Ast] = {
    val stmtNodeInfo = createDotNetNodeInfo(globalStatement.json(ParserKeys.Statement))
    stmtNodeInfo.node match
      // Denotes a top-level method declaration. These shall be added to the fictitious "main" created
      // by `astForTopLevelStatements`.
      case LocalFunctionStatement =>
        astForMethodDeclaration(stmtNodeInfo, extraModifiers = newModifierNode(ModifierTypes.STATIC) :: Nil)
      case _ => astForNode(stmtNodeInfo)
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
    val argsAst = Try(throwStmt.json(ParserKeys.Expression)).toOption match {
      case Some(_expr: ujson.Obj) => astForNode(createDotNetNodeInfo(_expr))
      case _                      => Seq.empty[Ast]
    }
    val throwCall = operatorCallNode(throwStmt, CSharpOperators.throws, Some(getTypeFullNameFromAstNode(argsAst)))
    Seq(callAst(throwCall, argsAst))
  }

  protected def astForTryStatement(tryStmt: DotNetNodeInfo): Seq[Ast] = {
    val tryNode          = controlStructureNode(tryStmt, ControlStructureTypes.TRY, code(tryStmt))
    val tryBlockNodeInfo = createDotNetNodeInfo(tryStmt.json(ParserKeys.Block))
    val tryAst           = astForBlock(tryBlockNodeInfo, Option(code(tryBlockNodeInfo)))

    val catchAsts = Try(tryStmt.json(ParserKeys.Catches))
      .map(_.arr.toSeq)
      .map { c =>
        c.map { value =>
          val nodeInfo  = createDotNetNodeInfo(value)
          val catchNode = controlStructureNode(nodeInfo, ControlStructureTypes.CATCH, code(nodeInfo))
          val children  = astForNode(nodeInfo)
          Ast(catchNode).withChildren(children)
        }
      }
      .getOrElse(Seq.empty)

    val finallyAst = Try(createDotNetNodeInfo(tryStmt.json(ParserKeys.Finally))).toOption.map { finallyNodeInfo =>
      val finallyNode      = controlStructureNode(finallyNodeInfo, ControlStructureTypes.FINALLY, code(finallyNodeInfo))
      val finallyClauseAst = astForFinallyClause(finallyNodeInfo)
      Ast(finallyNode).withChildren(finallyClauseAst)
    }

    val controlStructureAst = tryCatchAst(tryNode, tryAst, catchAsts, finallyAst)
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
    val tryNode = controlStructureNode(usingStmt, ControlStructureTypes.TRY, code(usingStmt))
    val declAst =
      Try(createDotNetNodeInfo(usingStmt.json(ParserKeys.Declaration))).map(astForNode).getOrElse(scala.Seq.empty[Ast])
    val tryNodeInfo = createDotNetNodeInfo(usingStmt.json(ParserKeys.Statement))
    val tryAst      = astForBlock(tryNodeInfo, Option("try"))

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
      val disposeAst  = callAst(disposeCall, receiver = Option(Ast(id)))
      val childrenAst = Ast(blockNode(usingStmt)).withChild(disposeAst)
      val finallyNode = controlStructureNode(usingStmt, ControlStructureTypes.FINALLY, "finally")
      Ast(finallyNode).withChild(childrenAst)
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
