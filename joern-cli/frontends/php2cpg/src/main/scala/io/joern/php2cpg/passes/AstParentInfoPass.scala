package io.joern.php2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, NamespaceBlock, Method, TypeDecl}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._

class AstParentInfoPass(cpg: Cpg) extends ConcurrentWriterCpgPass[AstNode](cpg) {

  override def generateParts(): Array[AstNode] = {
    (cpg.method ++ cpg.typeDecl).toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, node: AstNode): Unit = {
    findParent(node).foreach { parentNode =>
      val astParentType     = parentNode.label
      val astParentFullName = parentNode.property(PropertyNames.FULL_NAME)

      diffGraph.setNodeProperty(node, PropertyNames.AST_PARENT_TYPE, astParentType)
      diffGraph.setNodeProperty(node, PropertyNames.AST_PARENT_FULL_NAME, astParentFullName)
    }
  }

  private def hasValidContainingNodes(nodes: Iterator[AstNode]): Iterator[AstNode] = {
    nodes.collect {
      case m: Method         => m
      case t: TypeDecl       => t
      case n: NamespaceBlock => n
    }
  }

  def findParent(node: AstNode): Option[AstNode] = {
    node.start
      .repeat(_.astParent)(_.until(hasValidContainingNodes(_)).emit(hasValidContainingNodes(_)))
      .find(_ != node)
  }
}
