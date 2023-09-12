package io.joern.javasrc2cpg.astcreation.statements

import com.github.javaparser.ast.stmt.AssertStmt
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.stmt.BreakStmt
import com.github.javaparser.ast.stmt.CatchClause
import com.github.javaparser.ast.stmt.ContinueStmt
import com.github.javaparser.ast.stmt.DoStmt
import com.github.javaparser.ast.stmt.EmptyStmt
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.ast.stmt.ExpressionStmt
import com.github.javaparser.ast.stmt.ForEachStmt
import com.github.javaparser.ast.stmt.ForStmt
import com.github.javaparser.ast.stmt.IfStmt
import com.github.javaparser.ast.stmt.LabeledStmt
import com.github.javaparser.ast.stmt.ReturnStmt
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.stmt.SwitchEntry
import com.github.javaparser.ast.stmt.SwitchStmt
import com.github.javaparser.ast.stmt.SynchronizedStmt
import com.github.javaparser.ast.stmt.ThrowStmt
import com.github.javaparser.ast.stmt.TryStmt
import com.github.javaparser.ast.stmt.WhileStmt
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.astcreation.ExpectedType
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.newIdentifierNode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewControlStructure
import io.shiftleft.codepropertygraph.generated.nodes.NewJumpTarget
import io.shiftleft.codepropertygraph.generated.nodes.NewReturn
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

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
      case x: AssertStmt       => Seq(astForAssertStatement(x))
      case x: BlockStmt        => Seq(astForBlockStatement(x))
      case x: BreakStmt        => Seq(astForBreakStatement(x))
      case x: ContinueStmt     => Seq(astForContinueStatement(x))
      case x: DoStmt           => Seq(astForDo(x))
      case _: EmptyStmt        => Seq() // Intentionally skipping this
      case x: ExpressionStmt   => astsForExpression(x.getExpression, ExpectedType.Void)
      case x: ForEachStmt      => astForForEach(x)
      case x: ForStmt          => Seq(astForFor(x))
      case x: IfStmt           => Seq(astForIf(x))
      case x: LabeledStmt      => astsForLabeledStatement(x)
      case x: ReturnStmt       => Seq(astForReturnNode(x))
      case x: SwitchStmt       => Seq(astForSwitchStatement(x))
      case x: SynchronizedStmt => Seq(astForSynchronizedStatement(x))
      case x: ThrowStmt        => Seq(astForThrow(x))
      case x: TryStmt          => astsForTry(x)
      case x: WhileStmt        => Seq(astForWhile(x))
      // case x: LocalClassDeclarationStmt => Seq(astForLocalClassDeclarationStmt(x))
      case x =>
        logger.warn(s"Attempting to generate AST for unknown statement of type ${x.getClass}")
        Seq(unknownAst(x))
    }
  }

}
