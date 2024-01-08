package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.parser.{ResourceManagedParser, RubyNodeCreator}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate

import scala.util.{Failure, Success}

class AstCreator(protected val filename: String, parser: ResourceManagedParser, projectRoot: Option[String] = None)(
  implicit withSchemaValidation: ValidationMode
) extends AstCreatorBase(filename)
    with AstCreatorHelper
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstForFunctionsCreator
    with AstForTypesCreator
    with FreshVariableCreator
    with AstNodeBuilder[RubyNode, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected val relativeFileName: String =
    projectRoot.map(filename.stripPrefix).map(_.stripPrefix(java.io.File.separator)).getOrElse(filename)

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    parser.parse(filename) match
      case Success(programCtx) =>
        val rootNode = new RubyNodeCreator().visit(programCtx).asInstanceOf[StatementList]
        val ast      = astForRubyFile(rootNode)
        Ast.storeInDiffGraph(ast, diffGraph)
        diffGraph
      case Failure(exception) =>
        logger.warn(s"Could not parse file: $filename, skipping - ", exception)
        diffGraph
  }

  /* A Ruby file has the following AST hierarchy: FILE -> NAMESPACE_BLOCK -> METHOD.
   * The (parsed) contents of the file are put under that fictitious METHOD node, thus
   * allowing for a straightforward representation of out-of-method statements.
   */
  private def astForRubyFile(rootStatements: StatementList): Ast = {
    val fileNode = NewFile().name(relativeFileName)
    val namespaceBlock = NewNamespaceBlock()
      .filename(relativeFileName)
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(s"$relativeFileName:${NamespaceTraversal.globalNamespaceName}")

    methodAstParentStack.push(namespaceBlock)
    scope.pushNewScope(namespaceBlock)
    val rubyFileMethod = astInFakeMethod(rootStatements)
    scope.popScope()
    methodAstParentStack.pop()
    Ast(fileNode).withChild(Ast(namespaceBlock).withChild(rubyFileMethod))
  }

  private def astInFakeMethod(rootNode: StatementList): Ast = {
    val name     = ":program" // TODO: avoid this hardcoding. Move it into Defines?
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

    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)
    val statementAsts = rootNode.statements.flatMap(astsForStatement)
    val bodyAst       = blockAst(blockNode(rootNode), statementAsts)
    scope.popScope()
    methodAstParentStack.pop()

    methodAst(methodNode_, Seq.empty, bodyAst, methodReturn, newModifierNode(ModifierTypes.MODULE) :: Nil)
  }
}
