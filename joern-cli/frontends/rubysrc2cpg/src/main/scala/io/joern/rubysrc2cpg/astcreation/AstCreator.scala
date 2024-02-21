package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.datastructures.{BlockScope, NamespaceScope, RubyProgramSummary, RubyScope}
import io.joern.rubysrc2cpg.parser.{RubyNodeCreator, RubyParser}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

class AstCreator(
  val fileName: String,
  protected val programCtx: RubyParser.ProgramContext,
  protected val projectRoot: Option[String] = None,
  protected val programSummary: RubyProgramSummary = RubyProgramSummary()
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(fileName)
    with AstCreatorHelper
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstForFunctionsCreator
    with AstForTypesCreator
    with FreshVariableCreator
    with AstSummaryVisitor
    with AstNodeBuilder[RubyNode, AstCreator] {

  /* Used to track variable names and their LOCAL nodes.
   */
  protected val scope: RubyScope = new RubyScope(programSummary)

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected val relativeFileName: String =
    projectRoot.map(fileName.stripPrefix).map(_.stripPrefix(java.io.File.separator)).getOrElse(fileName)

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val rootNode = new RubyNodeCreator().visit(programCtx).asInstanceOf[StatementList]
    val ast      = astForRubyFile(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  /* A Ruby file has the following AST hierarchy: FILE -> NAMESPACE_BLOCK -> METHOD.
   * The (parsed) contents of the file are put under that fictitious METHOD node, thus
   * allowing for a straightforward representation of out-of-method statements.
   */
  private def astForRubyFile(rootStatements: StatementList): Ast = {
    val fileNode = NewFile().name(relativeFileName)
    val fullName = s"$relativeFileName:${NamespaceTraversal.globalNamespaceName}"
    val namespaceBlock = NewNamespaceBlock()
      .filename(relativeFileName)
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(fullName)

    scope.pushNewScope(NamespaceScope(fullName))
    val rubyFileMethod = astInFakeMethod(rootStatements)
    scope.popScope()

    Ast(fileNode).withChild(Ast(namespaceBlock).withChild(rubyFileMethod))
  }

  private def astInFakeMethod(rootNode: StatementList): Ast = {
    val name     = Defines.Program
    val fullName = computeMethodFullName(name)
    val code     = rootNode.text
    val methodNode_ = methodNode(
      node = rootNode,
      name = name,
      code = code,
      fullName = fullName,
      signature = None,
      fileName = relativeFileName
    )
    val methodReturn = methodReturnNode(rootNode, Defines.Any)

    scope.newProgramScope
      .map { moduleScope =>
        scope.pushNewScope(moduleScope)
        val block = blockNode(rootNode)
        scope.pushNewScope(BlockScope(block))
        val statementAsts = rootNode.statements.flatMap(astsForStatement)
        scope.popScope()
        val bodyAst = blockAst(block, statementAsts)
        scope.popScope()
        methodAst(methodNode_, Seq.empty, bodyAst, methodReturn, newModifierNode(ModifierTypes.MODULE) :: Nil)
      }
      .getOrElse(Ast())
  }
}
