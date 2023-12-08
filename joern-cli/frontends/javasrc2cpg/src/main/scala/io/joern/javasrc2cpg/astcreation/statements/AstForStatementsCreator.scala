package io.joern.javasrc2cpg.astcreation.statements

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

trait AstForStatementsCreator extends AstForSimpleStatementsCreator with AstForForLoopsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass())

  def astsForStatement(statement: Statement): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: LocalClassDeclarationStmt  => Seq()
    // case _: LocalRecordDeclarationStmt => Seq()
    // case _: YieldStmt                  => Seq()
    statement match {
      case x: ExplicitConstructorInvocationStmt =>
        Seq(astForExplicitConstructorInvocation(x))
      case x: AssertStmt                => Seq(astForAssertStatement(x))
      case x: BlockStmt                 => Seq(astForBlockStatement(x))
      case x: BreakStmt                 => Seq(astForBreakStatement(x))
      case x: ContinueStmt              => Seq(astForContinueStatement(x))
      case x: DoStmt                    => Seq(astForDo(x))
      case _: EmptyStmt                 => Seq() // Intentionally skipping this
      case x: ExpressionStmt            => astsForExpression(x.getExpression, ExpectedType.Void)
      case x: ForEachStmt               => astForForEach(x)
      case x: ForStmt                   => Seq(astForFor(x))
      case x: IfStmt                    => Seq(astForIf(x))
      case x: LabeledStmt               => astsForLabeledStatement(x)
      case x: ReturnStmt                => Seq(astForReturnNode(x))
      case x: SwitchStmt                => Seq(astForSwitchStatement(x))
      case x: SynchronizedStmt          => Seq(astForSynchronizedStatement(x))
      case x: ThrowStmt                 => Seq(astForThrow(x))
      case x: TryStmt                   => astsForTry(x)
      case x: WhileStmt                 => Seq(astForWhile(x))
      case x: LocalClassDeclarationStmt => Seq(astForLocalClassDeclaration(x))
      case x =>
        logger.warn(s"Attempting to generate AST for unknown statement of type ${x.getClass}")
        Seq(unknownAst(x))
    }
  }

}
