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
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.{SwitchEntryContext}
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewControlStructure, NewJumpTarget, NewReturn}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes}
import io.joern.x2cpg.utils.AstPropertiesUtil.*

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit
import org.slf4j.LoggerFactory

private case class PatternAstPartition(
  patternsIntroducedToBody: List[TypePatternExpr],
  patternsIntroducedToElse: List[TypePatternExpr],
  patternsIntroducedByStatement: List[TypePatternExpr]
)

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
        scope.enclosingMethod.map(_.getAndClearUnaddedPatternLocals()).getOrElse(Nil).map(Ast(_))
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

    val thisNode = identifierNode(stmt, NameConstants.This, NameConstants.This, typeFullName)
    scope.lookupVariable(NameConstants.This).variableNode.foreach { thisParam =>
      diffGraph.addEdge(thisNode, thisParam, EdgeTypes.REF)
    }
    val thisAst = Ast(thisNode)

    val initAst = Ast(callRoot)

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

  private[statements] def astsForDo(stmt: DoStmt): Seq[Ast] = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption

    val patternLocals = scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_))

    val patternPartition = partitionPatternAstsByScope(stmt)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val bodyAst = wrapInBlockWithPrefix(Nil, stmt.getBody)
    scope.popBlockAndHoistPatternVariables()

    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedByStatement)

    val code         = s"do {...} while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    patternLocals :+ doWhileAst(conditionAst, List(bodyAst), Some(code), lineNumber, columnNumber)
  }

  private[statements] def astsForWhile(stmt: WhileStmt): Seq[Ast] = {
    val conditionAst  = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption
    val patternLocals = scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_))

    val patternPartition = partitionPatternAstsByScope(stmt)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val bodyAst = wrapInBlockWithPrefix(Nil, stmt.getBody)
    scope.popBlockAndHoistPatternVariables()

    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedByStatement)

    val code         = s"while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    patternLocals :+ whileAst(conditionAst, List(bodyAst), Some(code), lineNumber, columnNumber)
  }

  private[statements] def astsForIf(stmt: IfStmt): Seq[Ast] = {

    val conditionAst =
      astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption.toList

    val patternLocals = scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_))

    val patternPartition = partitionPatternAstsByScope(stmt)

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToBody)
    val thenAst = stmt.getThenStmt match {
      case blockStmt: BlockStmt => astForBlockStatement(blockStmt)

      case stmt: Statement =>
        val thenStmts = astsForStatement(stmt)
        blockAst(blockNode(stmt), thenStmts.toList)
    }
    scope.popBlockAndHoistPatternVariables()

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternPartition.patternsIntroducedToElse)
    val elseAst = stmt.getElseStmt.toScala.map { elseStmt =>
      val elseBodyStatements = elseStmt match {
        case blockStmt: BlockStmt => blockStmt.getStatements.asScala
        case elseStmt: Statement  => elseStmt :: Nil
      }

      val elseBodyAsts = elseBodyStatements.flatMap(astsForStatement).toList
      val elseBlock    = blockAst(blockNode(elseStmt), elseBodyAsts)
      val elseNode     = controlStructureNode(elseStmt, ControlStructureTypes.ELSE, "else")
      controlStructureAst(elseNode, None, elseBlock :: Nil)
    }
    scope.popBlockAndHoistPatternVariables()

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

    patternLocals :+ astWithConditionEdge
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

    val selectorMustBeIdentifierOrFieldAccess =
      stmt.getEntries.asScala.flatMap(_.getLabels.asScala).exists(_.isPatternExpr)

    val (initializerAst, referenceAst) = if (selectorMustBeIdentifierOrFieldAccess) {
      val initAndRefAsts = initAndRefAstsForPatternInitializer(stmt.getSelector, selectorAst)
      (initAndRefAsts.get, Option(initAndRefAsts.get))
    } else {
      (selectorAst, None)
    }

    val entryAsts = stmt.getEntries.asScala.flatMap(astForSwitchEntry(_, referenceAst))

    val switchBodyAst = Ast(NewBlock()).withChildren(entryAsts)

    Ast(switchNode)
      .withChild(initializerAst)
      .withChild(switchBodyAst)
      .withConditionEdge(switchNode, selectorNode)
  }

  private[statements] def astForSynchronizedStatement(stmt: SynchronizedStmt): Ast = {
    val parentNode =
      NewBlock()
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))

    val modifier = Ast(modifierNode(stmt, "SYNCHRONIZED"))

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

  private def astForSwitchEntry(entry: SwitchEntry, selectorReferenceAst: Option[Ast]): Seq[Ast] = {
    // Fallthrough to/from a pattern is a compile error, so an entry can only have a pattern label if that is
    // the only label
    val labels    = entry.getLabels.asScala.toList
    val labelAsts = astsForSwitchLabels(labels, entry.isDefault)

    val entryContext = new SwitchEntryContext(entry, new CombinedTypeSolver())

    if (entry.getStatements.isEmpty) {
      labelAsts
    } else {
      scope.pushBlockScope()

      val instanceOfAst = labels.lastOption.collect { case patternExpr: PatternExpr =>
        selectorReferenceAst.map { selectorAst =>
          instanceOfAstForPattern(patternExpr, selectorAst)
        }
      }.flatten

      val patternsExposedToBody = entryContext.typePatternExprsExposedToChild(entry.getStatements.get(0)).asScala.toList
      scope.addLocalsForPatternsToEnclosingBlock(patternsExposedToBody)
      val patternAstsToAdd = scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_))

      val guardAst = entry.getGuard.toScala.map(astsForExpression(_, ExpectedType.Boolean))

      val statementsAst = guardAst match {
        case None => wrapInBlockWithPrefix(Nil, entry.getStatements.asScala.toList)

        case Some(guard) =>
          val bodyAst = wrapInBlockWithPrefix(Nil, entry.getStatements.asScala.toList)

          val ifNode = controlStructureNode(
            entry.getGuard.get(),
            ControlStructureTypes.IF,
            s"if (${guard.headOption.flatMap(_.rootCode)})"
          )

          controlStructureAst(ifNode, guard.headOption, bodyAst :: Nil)
      }
      scope.popBlockScope()

      val ifInstanceOfAst = instanceOfAst
        .map { instanceOfAst =>
          val ifNode = controlStructureNode(entry, ControlStructureTypes.IF, s"if (${instanceOfAst.rootCodeOrEmpty})")
          controlStructureAst(ifNode, Option(instanceOfAst), statementsAst :: Nil)
        }
        .getOrElse(statementsAst)

      val entryBodyAst = patternAstsToAdd match {
        case Nil  => ifInstanceOfAst
        case asts => blockAst(blockNode(entry), asts :+ ifInstanceOfAst)
      }

      labelAsts :+ entryBodyAst
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
