package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.datastructures.Scope
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.Ast
import org.slf4j.{Logger, LoggerFactory}
import ujson.Value

class AstCreator(val config: Config, val global: Global, diffGraph: DiffGraphBuilder, val parserResult: ParseResult)
    extends AstNodeBuilder
    with AstCreatorHelper {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode] = new Stack()

  def createAst(): Unit =
    Ast.storeInDiffGraph(astForFile(), diffGraph)

  private def astForFile(): Ast = {
    val name    = parserResult.filename
    val cpgFile = Ast(NewFile().name(name).order(0))
    cpgFile.withChild(astForParseResult())
  }

  protected def astsForNode(node: Value, order: Int): Seq[Ast] = nodeType(node) match {
    case BabelAst.File    => astsForNode(node("program"), order)
    case BabelAst.Program => astsForNodes(node("body").arr.toSeq)
    case _                => Seq(notHandledYet(node, order))
  }

  protected def astsForNodes(nodes: Seq[Value]): Seq[Ast] = withOrder(nodes) { (n, o) =>
    astsForNode(n, o)
  }.flatten

  private def createFakeMethod(name: String, fullName: String, path: String): Ast = {
    val allDecls      = Seq(parserResult.json("ast"))
    val lineNumber    = allDecls.headOption.flatMap(line)
    val lineNumberEnd = allDecls.lastOption.flatMap(lineEnd)

    val fakeGlobalTypeDecl = newTypeDecl(
      name,
      fullName,
      parserResult.filename,
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
    val declsAsts = allDecls.flatMap { node =>
      val r = astsForNode(node, currOrder)
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

  private def astForParseResult(): Ast = {
    val absolutePath = parserResult.fullPath
    val name         = NamespaceTraversal.globalNamespaceName
    val fullName     = MetaDataPass.getGlobalNamespaceBlockFullName(Some(absolutePath))
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(absolutePath)
      .order(1)
    methodAstParentStack.push(namespaceBlock)
    Ast(namespaceBlock).withChild(createFakeMethod(name, fullName, absolutePath))
  }

}
