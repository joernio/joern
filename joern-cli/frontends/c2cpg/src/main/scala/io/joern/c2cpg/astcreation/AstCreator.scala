package io.joern.c2cpg.astcreation

import io.joern.c2cpg.Config
import io.joern.c2cpg.datastructures.CGlobal
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack._
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/** Translates the Eclipse CDT AST into a CPG AST.
  */
class AstCreator(val filename: String, val config: Config, val global: CGlobal, val cdtAst: IASTTranslationUnit)
    extends AstCreatorBase(filename)
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

  def createAst(): DiffGraphBuilder = {
    val ast = astForTranslationUnit(cdtAst)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(iASTTranslationUnit: IASTTranslationUnit): Ast = {
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val ast = Ast(namespaceBlock).withChild(
      astInFakeMethod(namespaceBlock.fullName, absolutePath(filename), iASTTranslationUnit)
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
    val allDecls      = iASTTranslationUnit.getDeclarations
    val lineNumber    = allDecls.headOption.flatMap(line)
    val lineNumberEnd = allDecls.lastOption.flatMap(lineEnd)

    val name = NamespaceTraversal.globalNamespaceName
    val fakeGlobalTypeDecl = newTypeDecl(
      name,
      fullName,
      filename,
      name,
      NodeTypes.NAMESPACE_BLOCK,
      fullName,
      line = lineNumber,
      column = lineNumberEnd
    )

    methodAstParentStack.push(fakeGlobalTypeDecl)

    val fakeGlobalMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(path)
        .lineNumber(lineNumber)
        .lineNumberEnd(lineNumberEnd)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewScope(fakeGlobalMethod)

    val blockNode = NewBlock()
      .typeFullName("ANY")

    val declsAsts = allDecls.flatMap { stmt =>
      val r =
        CGlobal.getAstsFromAstCache(
          diffGraph,
          fileName(stmt),
          filename,
          line(stmt),
          column(stmt),
          astsForDeclaration(stmt)
        )
      r
    }.toIndexedSeq

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")

    Ast(fakeGlobalTypeDecl).withChild(
      Ast(fakeGlobalMethod)
        .withChild(Ast(blockNode).withChildren(declsAsts))
        .withChild(Ast(methodReturn))
    )
  }

}
