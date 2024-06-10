package io.joern.javasrc2cpg.astcreation.statements

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
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newModifierNode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewControlStructure, NewJumpTarget, NewReturn}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit

trait AstForSimpleStatementsCreator { this: AstCreator =>
  def astForBlockStatement(stmt: BlockStmt, codeStr: String = "<empty>", prefixAsts: Seq[Ast] = Seq.empty): Ast = {
    val block = NewBlock()
      .code(codeStr)
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))

    scope.pushBlockScope()

    val stmtAsts = stmt.getStatements.asScala.flatMap(astsForStatement)

    scope.popBlockScope()
    Ast(block)
      .withChildren(prefixAsts)
      .withChildren(stmtAsts)
  }

  private[statements] def astForExplicitConstructorInvocation(stmt: ExplicitConstructorInvocationStmt): Ast = {
    // TODO Handle super
    val maybeResolved = tryWithSafeStackOverflow(stmt.resolve())
    val args          = argAstsForCall(stmt, maybeResolved, stmt.getArguments)
    val argTypes      = argumentTypesForMethodLike(maybeResolved)

    val typeFullName = maybeResolved.toOption
      .map(_.declaringType())
      .flatMap(typ => scope.lookupType(typ.getName).orElse(typeInfoCalc.fullName(typ)))

    val callRoot = initNode(
      typeFullName.orElse(Some(TypeConstants.Any)),
      argTypes,
      args.size,
      stmt.toString,
      line(stmt),
      column(stmt)
    )

    val thisNode = newIdentifierNode(NameConstants.This, typeFullName.getOrElse(TypeConstants.Any))
    scope.lookupVariable(NameConstants.This).variableNode.foreach { thisParam =>
      diffGraph.addEdge(thisNode, thisParam, EdgeTypes.REF)
    }
    val thisAst = Ast(thisNode)

    val initAst = Ast(callRoot)

    // callAst(callRoot, args, Some(thisAst))
    scope.enclosingTypeDecl.foreach(
      _.registerInitToComplete(
        PartialInit(typeFullName.getOrElse(TypeConstants.Any), initAst, thisAst, args.toList, None)
      )
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

  private[statements] def astForDo(stmt: DoStmt): Ast = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption
    val stmtAsts     = astsForStatement(stmt.getBody)
    val code         = s"do {...} while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    doWhileAst(conditionAst, stmtAsts, Some(code), lineNumber, columnNumber)
  }

  private[statements] def astForWhile(stmt: WhileStmt): Ast = {
    val conditionAst = astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption
    val stmtAsts     = astsForStatement(stmt.getBody)
    val code         = s"while (${stmt.getCondition.toString})"
    val lineNumber   = line(stmt)
    val columnNumber = column(stmt)

    whileAst(conditionAst, stmtAsts, Some(code), lineNumber, columnNumber)
  }

  private[statements] def astForIf(stmt: IfStmt): Ast = {
    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .lineNumber(line(stmt))
        .columnNumber(column(stmt))
        .code(s"if (${stmt.getCondition.toString})")

    val conditionAst =
      astsForExpression(stmt.getCondition, ExpectedType.Boolean).headOption.toList

    val thenAsts = astsForStatement(stmt.getThenStmt)
    val elseAst  = astForElse(stmt.getElseStmt.toScala).toList

    val ast = Ast(ifNode)
      .withChildren(conditionAst)
      .withChildren(thenAsts)
      .withChildren(elseAst)

    conditionAst.flatMap(_.root.toList) match {
      case r :: Nil =>
        ast.withConditionEdge(ifNode, r)
      case _ =>
        ast
    }
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
    val switchNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.SWITCH)
        .code(s"switch(${stmt.getSelector.toString})")

    val selectorAsts = astsForExpression(stmt.getSelector, ExpectedType.empty)
    val selectorNode = selectorAsts.head.root.get

    val entryAsts = stmt.getEntries.asScala.flatMap(astForSwitchEntry)

    val switchBodyAst = Ast(NewBlock()).withChildren(entryAsts)

    Ast(switchNode)
      .withChildren(selectorAsts)
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

  private[statements] def astsForSwitchCases(entry: SwitchEntry): Seq[Ast] = {
    entry.getLabels.asScala.toList match {
      case Nil =>
        val target = NewJumpTarget()
          .name("default")
          .code("default")
        Seq(Ast(target))

      case labels =>
        labels.flatMap { label =>
          val jumpTarget = NewJumpTarget()
            .name("case")
            .code(label.toString)
          val labelAsts = astsForExpression(label, ExpectedType.empty).toList

          Ast(jumpTarget) :: labelAsts
        }
    }
  }

  private[statements] def astForSwitchEntry(entry: SwitchEntry): Seq[Ast] = {
    val labelAsts = astsForSwitchCases(entry)

    val statementAsts = entry.getStatements.asScala.flatMap(astsForStatement)

    labelAsts ++ statementAsts
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
