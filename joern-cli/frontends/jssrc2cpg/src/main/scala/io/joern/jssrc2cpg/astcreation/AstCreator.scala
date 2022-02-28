package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.datastructures.Scope
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.shiftleft.x2cpg.Ast
import org.slf4j.{Logger, LoggerFactory}

class AstCreator(
  val filename: String,
  val config: JsSrc2Cpg.Config,
  val diffGraph: DiffGraphBuilder,
  val parserResult: String
) extends AstNodeBuilder
    with AstCreatorHelper {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode] = new Stack()

  def createAst(): Unit =
    Ast.storeInDiffGraph(astForFile(parserResult), diffGraph)

  private def astForFile(parserResult: String): Ast = {
    println(parserResult)
    val cpgFile            = Ast(NewFile().name(filename).order(0))
    val translationUnitAst = astForTranslationUnit( /*parserResult*/ )
    cpgFile.withChild(translationUnitAst)
  }

  private def createFakeMethod(name: String, fullName: String, path: String /*, iASTTranslationUnit: String*/ ): Ast = {
    val allDecls      = Seq.empty[String]     // TODO iASTTranslationUnit.getDeclarations
    val lineNumber    = Option.empty[Integer] // TODO allDecls.headOption.flatMap(line)
    val lineNumberEnd = Option.empty[Integer] // TODO allDecls.lastOption.flatMap(lineEnd)

    val fakeGlobalTypeDecl = newTypeDecl(
      name,
      fullName,
      filename,
      name,
      NodeTypes.NAMESPACE_BLOCK,
      fullName,
      1,
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
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    var currOrder = 1
    val declsAsts = allDecls.flatMap { _ =>
      val r = Seq.empty[Ast] // TODO
      currOrder = currOrder + r.length
      r
    }.toIndexedSeq

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")
      .order(2)

    Ast(fakeGlobalTypeDecl).withChild(
      Ast(fakeGlobalMethod)
        .withChild(Ast(blockNode).withChildren(declsAsts))
        .withChild(Ast(methodReturn))
    )
  }

  private def astForTranslationUnit( /*iASTTranslationUnit: String*/ ): Ast = {
    val absolutePath =
      "" // TODO better.files.File(iASTTranslationUnit.getFilePath).path.toAbsolutePath.normalize().toString
    val name     = NamespaceTraversal.globalNamespaceName
    val fullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(absolutePath))
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(absolutePath)
      .order(1)
    methodAstParentStack.push(namespaceBlock)
    Ast(namespaceBlock).withChild(createFakeMethod(name, fullName, absolutePath /*, iASTTranslationUnit*/ ))
  }

}
