package io.joern.c2cpg.astcreation

import io.joern.c2cpg.Config
import io.joern.c2cpg.datastructures.CGlobal
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.NodeTypes
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack._
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/** Translates the Eclipse CDT AST into a CPG AST.
  */
class AstCreator(
  val filename: String,
  val config: Config,
  val cdtAst: IASTTranslationUnit,
  val file2OffsetTable: ConcurrentHashMap[String, Array[Int]]
) extends AstCreatorBase(filename)
    with AstForTypesCreator
    with AstForFunctionsCreator
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstNodeBuilder
    with AstCreatorHelper
    with MacroHandler {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()

  protected val usingDeclarationMappings: mutable.Map[String, String] = mutable.HashMap.empty

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode] = new Stack()

  override def absolutePath(filename: String): String = filename

  def createAst(): DiffGraphBuilder = {
    val ast = astForTranslationUnit(cdtAst)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(iASTTranslationUnit: IASTTranslationUnit): Ast = {
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val ast = Ast(namespaceBlock).withChild(
      astInFakeMethod(namespaceBlock.fullName, fileName(iASTTranslationUnit), iASTTranslationUnit)
    )
    if (config.includeComments) {
      val commentsAsts = cdtAst.getComments.map(comment => astForComment(comment)).toIndexedSeq
      ast.withChildren(commentsAsts)
    } else {
      ast
    }
  }

  /** Creates an AST of all declarations found in the translation unit - wrapped in a fake method.
    */
  private def astInFakeMethod(fullName: String, path: String, iASTTranslationUnit: IASTTranslationUnit): Ast = {
    val allDecls = iASTTranslationUnit.getDeclarations.toSeq
    val name     = NamespaceTraversal.globalNamespaceName

    val fakeGlobalTypeDecl =
      newTypeDeclNode(iASTTranslationUnit, name, fullName, filename, name, NodeTypes.NAMESPACE_BLOCK, fullName)
    methodAstParentStack.push(fakeGlobalTypeDecl)

    val fakeGlobalMethod =
      newMethodNode(iASTTranslationUnit, name, name, fullName, path, Option(NodeTypes.TYPE_DECL), Option(fullName))
    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewScope(fakeGlobalMethod)

    val blockNode = newBlockNode(iASTTranslationUnit, registerType(Defines.anyTypeName))

    val declsAsts = allDecls.flatMap { stmt =>
      CGlobal.getAstsFromAstCache(
        diffGraph,
        fileName(stmt),
        filename,
        line(stmt),
        column(stmt),
        astsForDeclaration(stmt)
      )
    }

    val methodReturn = newMethodReturnNode(iASTTranslationUnit, Defines.anyTypeName).code("RET")

    Ast(fakeGlobalTypeDecl).withChild(
      Ast(fakeGlobalMethod)
        .withChild(Ast(blockNode).withChildren(declsAsts))
        .withChild(Ast(methodReturn))
    )
  }

}
