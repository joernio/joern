package io.joern.c2cpg.astcreation

import io.joern.c2cpg.Config
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode, AstNodeBuilder as X2CpgAstNodeBuilder}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.eclipse.cdt.core.dom.ast.{IASTNode, IASTTranslationUnit}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/** Translates the Eclipse CDT AST into a CPG AST.
  */
class AstCreator(
  val filename: String,
  val global: Global,
  val config: Config,
  val cdtAst: IASTTranslationUnit,
  val file2OffsetTable: ConcurrentHashMap[String, Array[Int]]
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(filename)
    with AstForTypesCreator
    with AstForFunctionsCreator
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstNodeBuilder
    with AstCreatorHelper
    with MacroHandler
    with X2CpgAstNodeBuilder[IASTNode, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()

  protected val usingDeclarationMappings: mutable.Map[String, String] = mutable.HashMap.empty

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode] = new Stack()

  def createAst(): DiffGraphBuilder = {
    val ast = astForTranslationUnit(cdtAst)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(iASTTranslationUnit: IASTTranslationUnit): Ast = {
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val translationUnitAst =
      astInFakeMethod(namespaceBlock.fullName, fileName(iASTTranslationUnit), iASTTranslationUnit)
    val depsAndImportsAsts = astsForDependenciesAndImports(iASTTranslationUnit)
    val commentsAsts       = astsForComments(iASTTranslationUnit)
    val childrenAsts       = depsAndImportsAsts ++ Seq(translationUnitAst) ++ commentsAsts
    setArgumentIndices(childrenAsts)
    Ast(namespaceBlock).withChildren(childrenAsts)
  }

  /** Creates an AST of all declarations found in the translation unit - wrapped in a fake method.
    */
  private def astInFakeMethod(fullName: String, path: String, iASTTranslationUnit: IASTTranslationUnit): Ast = {
    val allDecls = iASTTranslationUnit.getDeclarations.toList.filterNot(isIncludedNode)
    val name     = NamespaceTraversal.globalNamespaceName

    val fakeGlobalTypeDecl =
      typeDeclNode(iASTTranslationUnit, name, fullName, filename, name, NodeTypes.NAMESPACE_BLOCK, fullName)
    methodAstParentStack.push(fakeGlobalTypeDecl)

    val fakeGlobalMethod =
      methodNode(iASTTranslationUnit, name, name, fullName, None, path, Option(NodeTypes.TYPE_DECL), Option(fullName))
    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewScope(fakeGlobalMethod)

    val blockNode_ = blockNode(iASTTranslationUnit, Defines.empty, registerType(Defines.anyTypeName))

    val declsAsts = allDecls.flatMap(astsForDeclaration)
    setArgumentIndices(declsAsts)

    val methodReturn = newMethodReturnNode(iASTTranslationUnit, Defines.anyTypeName)
    Ast(fakeGlobalTypeDecl).withChild(
      methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, declsAsts), methodReturn)
    )
  }
}
