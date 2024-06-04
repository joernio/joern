package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.passes.ConcurrentWriterCpgPass

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** This pass has MethodStubCreator and TypeDeclStubCreator as prerequisite for language frontends which do not provide
  * method stubs and type decl stubs.
  */
class ContainsEdgePass(cpg: Cpg) extends ConcurrentWriterCpgPass[AstNode](cpg) {
  import ContainsEdgePass._

  override def generateParts(): Array[AstNode] =
    cpg.graph.nodes(sourceTypes*).asScala.map(_.asInstanceOf[AstNode]).toArray

  override def runOnPart(dstGraph: DiffGraphBuilder, source: AstNode): Unit = {
    // AST is assumed to be a tree. If it contains cycles, then this will give a nice endless loop with OOM
    val queue = mutable.ArrayDeque[StoredNode](source)
    while (queue.nonEmpty) {
      val parent = queue.removeHead()
      for (nextNode <- parent._astOut) {
        if (isDestinationType(nextNode)) dstGraph.addEdge(source, nextNode, EdgeTypes.CONTAINS)
        if (!isSourceType(nextNode)) queue.append(nextNode)
      }
    }
  }
}

object ContainsEdgePass {

  private def isSourceType(node: StoredNode): Boolean = node match {
    case _: Method | _: TypeDecl | _: File => true
    case _                                 => false
  }

  private def isDestinationType(node: StoredNode): Boolean = node match {
    case _: Block | _: Identifier | _: FieldIdentifier | _: Return | _: Method | _: TypeDecl | _: Call | _: Literal |
        _: MethodRef | _: TypeRef | _: ControlStructure | _: JumpTarget | _: Unknown | _: TemplateDom =>
      true
    case _ => false
  }

  private val sourceTypes = List(NodeTypes.METHOD, NodeTypes.TYPE_DECL, NodeTypes.FILE)

}
