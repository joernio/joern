package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.scope.Scope
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.Ast
import org.slf4j.{Logger, LoggerFactory}
import ujson.Value

class AstCreator(val config: Config, val diffGraph: DiffGraphBuilder, val parserResult: ParseResult)
    extends AstNodeBuilder
    with AstCreatorHelper {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode] = new Stack[NewNode]()
  protected val localAstParentStack: Stack[NewBlock] = new Stack[NewBlock]()

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

  private def createProgramMethod(path: String): Ast = {
    val allDecls     = Seq(parserResult.json("ast"))
    val lineNumber   = allDecls.headOption.flatMap(line)
    val columnNumber = allDecls.lastOption.flatMap(column)
    val name         = ":program"
    val fullName     = parserResult.filename + ":" + name

    val programMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(path)
        .lineNumber(lineNumber)
        .lineNumberEnd(columnNumber)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    // We always create an instance parameter because in JS every function could get called with an instance.
    val thisParam = createParameterInNode("this", "this", 0, lineNumber, columnNumber)

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

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(programMethod, methodAstParentStack.head, name, fullName, path)

    functionTypeAndTypeDeclAst.withChild(
      Ast(programMethod)
        .withChild(Ast(thisParam))
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
    Ast(namespaceBlock).withChild(createProgramMethod(absolutePath))
  }

}
