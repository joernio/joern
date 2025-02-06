package io.joern.javasrc2cpg.astcreation.statements

import com.github.javaparser.ast.expr.TypePatternExpr
import com.github.javaparser.ast.stmt.{
  AssertStmt,
  BlockStmt,
  BreakStmt,
  ContinueStmt,
  DoStmt,
  EmptyStmt,
  ExplicitConstructorInvocationStmt,
  ExpressionStmt,
  ForEachStmt,
  ForStmt,
  IfStmt,
  LabeledStmt,
  ReturnStmt,
  Statement,
  SwitchStmt,
  SynchronizedStmt,
  ThrowStmt,
  TryStmt,
  WhileStmt
}
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.x2cpg.Ast
import org.slf4j.LoggerFactory
import com.github.javaparser.ast.stmt.LocalClassDeclarationStmt
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.{
  DoStatementContext,
  ForStatementContext,
  IfStatementContext,
  StatementContext,
  WhileStatementContext
}
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.joern.javasrc2cpg.scope.PatternVariableInfo

import java.util
import java.util.Collections
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait AstForStatementsCreator extends AstForSimpleStatementsCreator with AstForForLoopsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass())

  def astsForStatement(statement: Statement): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: LocalClassDeclarationStmt  => Seq()
    // case _: LocalRecordDeclarationStmt => Seq()
    // case _: YieldStmt                  => Seq()
    val statementAsts = statement match {
      case x: ExplicitConstructorInvocationStmt =>
        Seq(astForExplicitConstructorInvocation(x))
      case x: AssertStmt                => Seq(astForAssertStatement(x))
      case x: BlockStmt                 => Seq(astForBlockStatement(x))
      case x: BreakStmt                 => Seq(astForBreakStatement(x))
      case x: ContinueStmt              => Seq(astForContinueStatement(x))
      case x: DoStmt                    => astsForDo(x)
      case _: EmptyStmt                 => Seq() // Intentionally skipping this
      case x: ExpressionStmt            => astsForExpression(x.getExpression, ExpectedType.Void)
      case x: ForEachStmt               => astForForEach(x)
      case x: ForStmt                   => astsForFor(x)
      case x: IfStmt                    => astsForIf(x)
      case x: LabeledStmt               => astsForLabeledStatement(x)
      case x: ReturnStmt                => Seq(astForReturnNode(x))
      case x: SwitchStmt                => Seq(astForSwitchStatement(x))
      case x: SynchronizedStmt          => Seq(astForSynchronizedStatement(x))
      case x: ThrowStmt                 => Seq(astForThrow(x))
      case x: TryStmt                   => astsForTry(x)
      case x: WhileStmt                 => astsForWhile(x)
      case x: LocalClassDeclarationStmt => Seq(astForLocalClassDeclaration(x))
      case x =>
        logger.warn(s"Attempting to generate AST for unknown statement of type ${x.getClass}")
        Seq(unknownAst(x))
    }

    val patternVariableAsts =
      scope.enclosingMethod
        .map { enclosingMethod => enclosingMethod.getAndClearUnaddedPatternLocals().map(Ast(_)) }
        .getOrElse(Nil)
    patternVariableAsts ++ statementAsts
  }

  private[statements] def partitionPatternAstsByScope(
    stmt: IfStmt | WhileStmt | ForStmt | DoStmt
  ): PatternAstPartition = {
    val typeSolver = new CombinedTypeSolver()
    val (context, body) = stmt match {
      case ifStmt: IfStmt       => (new IfStatementContext(ifStmt, typeSolver), ifStmt.getThenStmt)
      case whileStmt: WhileStmt => (new WhileStatementContext(whileStmt, typeSolver), whileStmt.getBody)
      case forStmt: ForStmt     => (new ForStatementContext(forStmt, typeSolver), forStmt.getBody)
      case doStmt: DoStmt       => (new DoStatementContext(doStmt, typeSolver), doStmt.getBody)
    }

    val patternsIntroducedByStmt =
      Collections.newSetFromMap(new util.IdentityHashMap[TypePatternExpr, java.lang.Boolean]())
    patternsIntroducedByStmt.addAll(context.getIntroducedTypePatterns)

    val patternsIntroducedToBody =
      Collections.newSetFromMap(new util.IdentityHashMap[TypePatternExpr, java.lang.Boolean](2))

    patternsIntroducedToBody.addAll(context.typePatternExprsExposedToChild(body))

    val patternsIntroducedToElse =
      Collections.newSetFromMap(new util.IdentityHashMap[TypePatternExpr, java.lang.Boolean]())

    context.getWrappedNode match {
      case ifStmt: IfStmt if ifStmt.getElseStmt.isPresent =>
        patternsIntroducedToElse.addAll(context.typePatternExprsExposedToChild(ifStmt.getElseStmt.get()))
      case _ => // Nothing to do in this case
    }

    PatternAstPartition(
      patternsIntroducedToBody.asScala.toList,
      patternsIntroducedToElse.asScala.toList,
      patternsIntroducedByStmt.asScala.toList
    )
  }

  private[statements] def wrapInBlockWithPrefix(prefixAsts: List[Ast], stmt: Statement): Ast = {
    wrapInBlockWithPrefix(prefixAsts, stmt :: Nil)
  }

  private[statements] def wrapInBlockWithPrefix(prefixAsts: List[Ast], stmts: List[Statement]): Ast = {
    stmts match {
      case Seq()                     => Ast(NewBlock()).withChildren(prefixAsts)
      case Seq(blockStmt: BlockStmt) => astForBlockStatement(blockStmt, prefixAsts = prefixAsts)

      case Seq(singleStmt) =>
        val stmtAsts = astsForStatement(singleStmt)
        stmtAsts.toList match {
          case bodyStmt :: Nil if prefixAsts.isEmpty => bodyStmt
          case _                                     => blockAst(blockNode(singleStmt), prefixAsts ++ stmtAsts)
        }

      case _ =>
        val stmtsAsts = stmts.flatMap(astsForStatement)
        stmtsAsts match {
          case Nil                                   => Ast(NewBlock()).withChildren(prefixAsts)
          case bodyStmt :: Nil if prefixAsts.isEmpty => bodyStmt
          case _                                     => blockAst(blockNode(stmts.head), prefixAsts ++ stmtsAsts)
        }
    }
  }

}
