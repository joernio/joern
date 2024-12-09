package io.joern.javasrc2cpg.astcreation.statements

import com.github.javaparser.ast.expr.{Expression, PatternExpr, TypePatternExpr}
import com.github.javaparser.ast.stmt.{
  AssertStmt,
  BlockStmt,
  BreakStmt,
  CatchClause,
  ContinueStmt,
  DoStmt,
  ExplicitConstructorInvocationStmt,
  IfStmt,
  LabeledStmt,
  ReturnStmt,
  Statement,
  SwitchEntry,
  SwitchStmt,
  SynchronizedStmt,
  ThrowStmt,
  TryStmt,
  WhileStmt
}
import com.github.javaparser.symbolsolver.javaparsermodel.PatternVariableVisitor
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.{
  DoStatementContext,
  ExpressionContext,
  IfStatementContext,
  SwitchEntryContext,
  WhileStatementContext
}
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.PatternVariableInfo
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newModifierNode}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewIdentifier,
  NewJumpTarget,
  NewReturn
}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes}
import io.joern.x2cpg.utils.AstPropertiesUtil.*

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit
import io.joern.javasrc2cpg.scope.Scope.NewVariableNode
import org.slf4j.LoggerFactory

private case class PatternAstPartition(
  patternsIntroducedToBody: List[TypePatternExpr],
  patternsIntroducedToElse: List[TypePatternExpr],
  patternsIntroducedByStatement: List[TypePatternExpr],
  astsAddedBeforeStatement: List[Ast],
  astsAddedToBody: List[Ast],
  astsAddedToElse: List[Ast],
  astsAddedAfterStatement: List[Ast]
) {
  val addedAsts: Set[Ast] =
    (astsAddedBeforeStatement ++ astsAddedToBody ++ astsAddedToElse ++ astsAddedAfterStatement).toSet
}

trait AstForSimpleStatementsCreator { this: AstCreator =>
  private val logger = LoggerFactory.getLogger(this.getClass)

  def astForBlockStatement(
    stmt: BlockStmt,
    codeStr: String = "<empty>",
    prefixAsts: Seq[Ast] = Seq.empty,
    includeTemporaryLocals: Boolean = false
  ): Ast = {
    val block = NewBlock()
      .code(codeStr)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    scope.pushBlockScope()

    val stmtAsts = stmt.getStatements.asScala.flatMap(astsForStatement)

    val temporaryLocalAsts =
      if (includeTemporaryLocals)
        scope.enclosingMethod.map(_.getTemporaryLocals).getOrElse(Nil).map(Ast(_))
      else
        Nil

    scope.popBlockScope()
    Ast(block)
      .withChildren(temporaryLocalAsts)
      .withChildren(prefixAsts)
      .withChildren(stmtAsts)
  }

  private[statements] def astForExplicitConstructorInvocation(stmt: ExplicitConstructorInvocationStmt): Ast = {
    // TODO Handle super
    val maybeResolved = tryWithSafeStackOverflow(stmt.resolve())
    val args          = argAstsForCall(stmt, maybeResolved, stmt.getArguments)
    val argTypes      = argumentTypesForMethodLike(maybeResolved.toOption)

    // TODO: We can do better than defaultTypeFallback() for the fallback type by looking at the enclosing
    //  type decl name or `extends X` name for `this` and `super` calls respectively.
    val typeFullName = maybeResolved.toOption
      .map(_.declaringType())
      .flatMap(typ => scope.lookupType(typ.getName).orElse(typeInfoCalc.fullName(typ)))
      .getOrElse(defaultTypeFallback())

    val callRoot = initNode(Option(typeFullName), argTypes, args.size, stmt.toString, line(stmt), column(stmt))

    val thisNode = newIdentifierNode(NameConstants.This, typeFullName)
    scope.lookupVariable(NameConstants.This).variableNode.foreach { thisParam =>
      diffGraph.addEdge(thisNode, thisParam, EdgeTypes.REF)
    }
    val thisAst = Ast(thisNode)

    val initAst = Ast(callRoot)

    // callAst(callRoot, args, Some(thisAst))
    scope.enclosingTypeDecl.foreach(
      _.registerInitToComplete(PartialInit(typeFullName, initAst, thisAst, args.toList, None))
    )
    initAst
  }

  private[statements] def astForAssertStatement(stmt: AssertStmt): Ast = {
    val callNode = NewCall()
      .name("assert")
      .methodFullName("assert")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(code(stmt))
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    val args = astsForExpression(stmt.getCheck, ExpectedType.Boolean)
    callAst(callNode, args)
  }

  private[statements] def astForBreakStatement(stmt: BreakStmt): Ast = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(code(stmt))
    Ast(node)
  }

  private[statements] def astForContinueStatement(stmt: ContinueStmt): Ast = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(code(stmt))
    Ast(node)
  }

  private[statements] def astsForDo(stmt: DoStmt): List[Ast] = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption

    val doContext = new DoStatementContext(stmt, new CombinedTypeSolver())

    val patternPartition = partitionPatternAstsByScope(doContext)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val bodyAst = wrapInBlockWithPrefix(patternPartition.astsAddedToBody, stmt.getBody)
    scope.popBlockScope()

    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedByStatement)

    val code         = s"do {...} while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    val ast = doWhileAst(conditionAst, List(bodyAst), Some(code), lineNumber, columnNumber)
    patternPartition.astsAddedBeforeStatement ++ (ast :: patternPartition.astsAddedAfterStatement)
  }

  private[statements] def astsForWhile(stmt: WhileStmt): List[Ast] = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption

    val whileContext = new WhileStatementContext(stmt, new CombinedTypeSolver())

    val patternPartition = partitionPatternAstsByScope(whileContext)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val bodyAst = wrapInBlockWithPrefix(patternPartition.astsAddedToBody, stmt.getBody)
    scope.popBlockScope()

    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedByStatement)

    val code         = s"while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    val ast = whileAst(conditionAst, List(bodyAst), Some(code), lineNumber, columnNumber)
    patternPartition.astsAddedBeforeStatement ++ (ast :: patternPartition.astsAddedAfterStatement)
  }

  private[statements] def astsForIf(stmt: IfStmt): Seq[Ast] = {

    val conditionAst =
      astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption.toList

    val ifContext = new IfStatementContext(stmt, new CombinedTypeSolver())

    val patternPartition = partitionPatternAstsByScope(ifContext)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val thenAst = stmt.getThenStmt match {
      case blockStmt: BlockStmt => astForBlockStatement(blockStmt, prefixAsts = patternPartition.astsAddedToBody)

      case stmt: Statement =>
        val elseStmts = astsForStatement(stmt)
        blockAst(blockNode(stmt), patternPartition.astsAddedToBody ++ elseStmts)
    }
    scope.popBlockScope()

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToElse)
    val elseAst = stmt.getElseStmt.toScala.map { elseStmt =>
      val elseBodyStatements = elseStmt match {
        case blockStmt: BlockStmt => blockStmt.getStatements.asScala
        case elseStmt: Statement  => elseStmt :: Nil
      }

      val elseBodyAsts = elseBodyStatements.flatMap(astsForStatement)
      val elseBlock    = blockAst(blockNode(elseStmt), patternPartition.astsAddedToElse ++ elseBodyAsts)
      val elseNode     = controlStructureNode(elseStmt, ControlStructureTypes.ELSE, "else")
      controlStructureAst(elseNode, None, elseBlock :: Nil)
    }
    scope.popBlockScope()

    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"if (${conditionAst.headOption.flatMap(_.rootCode).getOrElse("")})")

    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedByStatement)
    val ast = Ast(ifNode)
      .withChildren(conditionAst)
      .withChild(thenAst)
      .withChildren(elseAst.toList)

    val astWithConditionEdge = conditionAst.flatMap(_.root.toList) match {
      case r :: Nil =>
        ast.withConditionEdge(ifNode, r)
      case _ =>
        ast
    }

    patternPartition.astsAddedBeforeStatement ++ (astWithConditionEdge :: patternPartition.astsAddedAfterStatement)
  }

  private[statements] def astForElse(maybeStmt: Option[Statement]): Option[Ast] = {
    maybeStmt.map { stmt =>
      val elseAsts = astsForStatement(stmt)

      val elseNode =
        NewControlStructure()
          .controlStructureType(ControlStructureTypes.ELSE)
          .lineNumber(line(stmt))
          .columnNumber(column(stmt))
          .code("else")

      Ast(elseNode).withChildren(elseAsts)
    }
  }

  private[statements] def astForSwitchStatement(stmt: SwitchStmt): Ast = {
    // TODO: Add support for switch expressions
    // TODO: Switch expressions should either be represented with MATCH or we should add a break.
    val switchNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.SWITCH)
        .code(s"switch(${stmt.getSelector.toString})")

    val selectorAst = astsForExpression(stmt.getSelector, ExpectedType.empty) match {
      case Seq() =>
        throw new IllegalArgumentException(s"Got an empty ast list for expression ${code(stmt.getSelector)}")

      case Seq(ast) => ast

      case asts =>
        logger.warn(s"Found multiple asts for selector expression ${code(stmt.getSelector)}")
        asts.head
    }

    val selectorNode = selectorAst.root.get

    val selectorMustBeIdentifier = stmt.getEntries.asScala.flatMap(_.getLabels.asScala).exists(_.isPatternExpr)

    val (selectorInitializer, selectorIdentifier, selectorRefsTo) = if (selectorMustBeIdentifier) {
      val (init, ident, refs) = astIdentifierAndRefsForPatternLhs(stmt.getSelector, selectorAst)
      (init, Option(ident), refs)
    } else {
      (selectorAst, None, None)
    }

    val entryAsts = stmt.getEntries.asScala.flatMap(astForSwitchEntry(_, selectorIdentifier, selectorRefsTo))

    val switchBodyAst = Ast(NewBlock()).withChildren(entryAsts)

    Ast(switchNode)
      .withChild(selectorInitializer)
      .withChild(switchBodyAst)
      .withConditionEdge(switchNode, selectorNode)
  }

  private[statements] def astForSynchronizedStatement(stmt: SynchronizedStmt): Ast = {
    val parentNode =
      NewBlock()
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val modifier = Ast(newModifierNode("SYNCHRONIZED"))

    val exprAsts = astsForExpression(stmt.getExpression, ExpectedType.empty)
    val bodyAst  = astForBlockStatement(stmt.getBody)

    Ast(parentNode)
      .withChild(modifier)
      .withChildren(exprAsts)
      .withChild(bodyAst)
  }

  private def astsForSwitchLabels(labels: List[Expression], isDefault: Boolean): Seq[Ast] = {
    val defaultAst = Option.when(labels.isEmpty || isDefault) {
      val target = NewJumpTarget()
        .name("default")
        .code("default")
      Ast(target)
    }

    val explicitLabelAsts = labels.flatMap { label =>
      val jumpTarget = NewJumpTarget()
        .name("case")
        .code(code(label))
      val labelAsts =
        if (label.isPatternExpr)
          Nil
        else
          astsForExpression(label, ExpectedType.empty).toList

      Ast(jumpTarget) :: labelAsts
    }

    (defaultAst ++ explicitLabelAsts).toList
  }

  private def astForSwitchEntry(
    entry: SwitchEntry,
    selectorIdentifier: Option[NewIdentifier],
    selectorRefsTo: Option[NewVariableNode]
  ): Seq[Ast] = {
    // Fallthrough to/from a pattern is a compile error, so an entry can only have a pattern label if that is
    // the only label
    val labels    = entry.getLabels.asScala.toList
    val labelAsts = astsForSwitchLabels(labels, entry.isDefault)

    val entryContext = new SwitchEntryContext(entry, new CombinedTypeSolver())

    val instanceOfAst = labels.lastOption.collect { case patternExpr: PatternExpr =>
      selectorIdentifier.map { selector =>
        astForInstanceOfWithPattern(patternExpr, Ast(selector), patternExpr)
      }
    }.flatten

    // TODO: Add variable local and assignment to entry body even if there are no statements
    if (entry.getStatements.isEmpty) {
      labelAsts
    } else {
      scope.pushBlockScope()
      val patternsExposedToBody = entryContext.typePatternExprsExposedToChild(entry.getStatements.get(0)).asScala.toList
      scope.addLocalsForPatternsToEnclosingBlock(patternsExposedToBody)
      val patternAstsToAdd = patternsExposedToBody
        .flatMap(typePattern => scope.enclosingMethod.get.getPatternVariableInfo(typePattern))
        .flatMap { case PatternVariableInfo(typePatternExpr, patternLocal, initializerAst, _, _) =>
          scope.enclosingMethod.get.registerPatternVariableInitializerToBeAddedToGraph(typePatternExpr)
          scope.enclosingMethod.get.registerPatternVariableLocalToBeAddedToGraph(typePatternExpr)
          Ast(patternLocal) :: initializerAst :: Nil
        }

      val guardAst = entry.getGuard.toScala.map(astsForExpression(_, ExpectedType.Boolean))

      val statementsAst = guardAst match {
        case None => wrapInBlockWithPrefix(patternAstsToAdd, entry.getStatements.asScala.toList)

        case Some(guard) =>
          val bodyAst = wrapInBlockWithPrefix(Nil, entry.getStatements.asScala.toList)

          val ifNode = controlStructureNode(
            entry.getGuard.get(),
            ControlStructureTypes.IF,
            s"if (${guard.headOption.flatMap(_.rootCode)})"
          )

          val ifAst = controlStructureAst(ifNode, guard.headOption, bodyAst :: Nil)
          patternAstsToAdd match {
            case Nil => ifAst
            case _   => blockAst(blockNode(entry)).withChildren(patternAstsToAdd).withChild(ifAst)
          }
      }
      scope.popBlockScope()

      instanceOfAst
        .map { instanceOfAst =>
          val ifNode = controlStructureNode(entry, ControlStructureTypes.IF, s"if (${instanceOfAst.rootCodeOrEmpty})")
          labelAsts :+ Ast(ifNode).withChild(instanceOfAst).withChild(statementsAst)
        }
        .getOrElse(labelAsts :+ statementsAst)
    }
  }

  private[statements] def astForReturnNode(ret: ReturnStmt): Ast = {
    val returnNode = NewReturn()
      .lineNumber(line(ret))
      .columnNumber(column(ret))
      .code(code(ret))
    if (ret.getExpression.isPresent) {
      val expectedType = scope.enclosingMethodReturnType.getOrElse(ExpectedType.empty)
      val exprAsts     = astsForExpression(ret.getExpression.get(), expectedType)
      returnAst(returnNode, exprAsts)
    } else {
      Ast(returnNode)
    }
  }

  private[statements] def astsForLabeledStatement(stmt: LabeledStmt): Seq[Ast] = {
    val jumpTargetAst = Ast(NewJumpTarget().name(stmt.getLabel.toString))
    val stmtAst       = astsForStatement(stmt.getStatement).toList

    jumpTargetAst :: stmtAst
  }

  private[statements] def astForThrow(stmt: ThrowStmt): Ast = {
    val throwNode = NewCall()
      .name("<operator>.throw")
      .methodFullName("<operator>.throw")
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .code(code(stmt))
      .dispatchType(DispatchTypes.STATIC_DISPATCH)

    val args = astsForExpression(stmt.getExpression, ExpectedType.empty)

    callAst(throwNode, args)
  }

  private[statements] def astForCatchClause(catchClause: CatchClause): Ast = {
    astForBlockStatement(catchClause.getBody)
  }

  private[statements] def astsForTry(stmt: TryStmt): Seq[Ast] = {
    val tryNode   = controlStructureNode(stmt, ControlStructureTypes.TRY, "try")
    val resources = stmt.getResources.asScala.flatMap(astsForExpression(_, expectedType = ExpectedType.empty)).toList

    val tryAst = astForBlockStatement(stmt.getTryBlock, codeStr = "try")
    val catchAsts = stmt.getCatchClauses.asScala.toList.map { catchClause =>
      val catchNode = controlStructureNode(catchClause, ControlStructureTypes.CATCH, "catch")
      Ast(catchNode).withChild(astForCatchClause(catchClause))
    }
    val finallyAst = stmt.getFinallyBlock.toScala.map { finallyBlock =>
      val finallyNode = controlStructureNode(finallyBlock, ControlStructureTypes.FINALLY, "finally")
      Ast(finallyNode).withChild(astForBlockStatement(finallyBlock, "finally"))
    }
    val controlStructureAst = tryCatchAst(tryNode, tryAst, catchAsts, finallyAst)
    resources.appended(controlStructureAst)
  }
}
