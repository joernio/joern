package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.nodes.AstNode.PropertyDefaults
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import overflowdb.SchemaViolationException

case class AstEdge(src: NewNode, dst: NewNode)

enum ValidationMode {
  case Enabled, Disabled
}

object Ast {

  private val logger = LoggerFactory.getLogger(getClass)

  def apply(node: NewNode)(implicit withSchemaValidation: ValidationMode): Ast = Ast(Vector.empty :+ node)
  def apply()(implicit withSchemaValidation: ValidationMode): Ast              = new Ast(Vector.empty)

  /** Copy nodes/edges of given `AST` into the given `diffGraph`.
    */
  def storeInDiffGraph(ast: Ast, diffGraph: DiffGraphBuilder): Unit = {

    setOrderWhereNotSet(ast)

    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.receiverEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.RECEIVER)
    }
    ast.refEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.REF)
    }

    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }

    ast.bindsEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.BINDS)
    }
  }

  def neighbourValidation(src: NewNode, dst: NewNode, edge: String)(implicit
    withSchemaValidation: ValidationMode
  ): Unit = if (
    withSchemaValidation == ValidationMode.Enabled &&
    !(src.isValidOutNeighbor(edge, dst) && dst.isValidInNeighbor(edge, src))
  ) {
    throw new SchemaViolationException(
      s"Malformed AST detected: (${src.label}) -[$edge]-> (${dst.label}) violates the schema."
    )
  }

  /** For all `order` fields that are unset, derive the `order` field automatically by determining the position of the
    * child among its siblings.
    */
  private def setOrderWhereNotSet(ast: Ast): Unit = {
    ast.root.collect { case r: AstNodeNew =>
      if (r.order == PropertyDefaults.Order) {
        r.order = 1
      }
    }
    val siblings = ast.edges.groupBy(_.src).map { case (_, edgeToChild) => edgeToChild.map(_.dst) }
    siblings.foreach { children =>
      children.zipWithIndex.collect { case (c: AstNodeNew, i) =>
        if (c.order == PropertyDefaults.Order) {
          c.order = i + 1
        }
      }
    }
  }

}

case class Ast(
  nodes: collection.Seq[
    NewNode
  ], // technically this should be a Seq[AstNewNode], but we also use it for non-ast nodes like Binding...
  edges: collection.Seq[AstEdge] = Vector.empty,
  conditionEdges: collection.Seq[AstEdge] = Vector.empty,
  refEdges: collection.Seq[AstEdge] = Vector.empty,
  bindsEdges: collection.Seq[AstEdge] = Vector.empty,
  receiverEdges: collection.Seq[AstEdge] = Vector.empty,
  argEdges: collection.Seq[AstEdge] = Vector.empty
)(implicit withSchemaValidation: ValidationMode = ValidationMode.Disabled) {

  def root: Option[NewNode] = nodes.headOption

  def rightMostLeaf: Option[NewNode] = nodes.lastOption

  /** AST that results when adding `other` as a child to this AST. `other` is connected to this AST's root node.
    */
  def withChild(other: Ast): Ast = {
    Ast(
      nodes ++ other.nodes,
      edges = edges ++ other.edges ++ root.toList.flatMap(r =>
        other.root.toList.map { rc =>
          Ast.neighbourValidation(r, rc, EdgeTypes.AST)
          AstEdge(r, rc)
        }
      ),
      conditionEdges = conditionEdges ++ other.conditionEdges,
      argEdges = argEdges ++ other.argEdges,
      receiverEdges = receiverEdges ++ other.receiverEdges,
      refEdges = refEdges ++ other.refEdges,
      bindsEdges = bindsEdges ++ other.bindsEdges
    )
  }

  def merge(other: Ast): Ast = {
    Ast(
      nodes ++ other.nodes,
      edges = edges ++ other.edges,
      conditionEdges = conditionEdges ++ other.conditionEdges,
      argEdges = argEdges ++ other.argEdges,
      receiverEdges = receiverEdges ++ other.receiverEdges,
      refEdges = refEdges ++ other.refEdges,
      bindsEdges = bindsEdges ++ other.bindsEdges
    )
  }

  /** AST that results when adding all ASTs in `asts` as children, that is, connecting them to the root node of this
    * AST.
    */
  def withChildren(asts: collection.Seq[Ast]): Ast = {
    if (asts.isEmpty) {
      this
    } else {
      // we do this iteratively as a recursive solution which will fail with
      // a StackOverflowException if there are too many elements in .tail.
      var ast = withChild(asts.head)
      asts.tail.foreach(c => ast = ast.withChild(c))
      ast
    }
  }

  def withConditionEdge(src: NewNode, dst: NewNode): Ast = {
    Ast.neighbourValidation(src, dst, EdgeTypes.CONDITION)
    this.copy(conditionEdges = conditionEdges ++ List(AstEdge(src, dst)))
  }

  def withRefEdge(src: NewNode, dst: NewNode): Ast = {
    Ast.neighbourValidation(src, dst, EdgeTypes.REF)
    this.copy(refEdges = refEdges ++ List(AstEdge(src, dst)))
  }

  def withBindsEdge(src: NewNode, dst: NewNode): Ast = {
    Ast.neighbourValidation(src, dst, EdgeTypes.BINDS)
    this.copy(bindsEdges = bindsEdges ++ List(AstEdge(src, dst)))
  }

  def withReceiverEdge(src: NewNode, dst: NewNode): Ast = {
    Ast.neighbourValidation(src, dst, EdgeTypes.RECEIVER)
    this.copy(receiverEdges = receiverEdges ++ List(AstEdge(src, dst)))
  }

  def withArgEdge(src: NewNode, dst: NewNode): Ast = {
    Ast.neighbourValidation(src, dst, EdgeTypes.ARGUMENT)
    this.copy(argEdges = argEdges ++ List(AstEdge(src, dst)))
  }

  def withArgEdges(src: NewNode, dsts: Seq[NewNode]): Ast = {
    dsts.foreach(dst => Ast.neighbourValidation(src, dst, EdgeTypes.ARGUMENT))
    this.copy(argEdges = argEdges ++ dsts.map(AstEdge(src, _)))
  }

  def withArgEdges(src: NewNode, dsts: Seq[NewNode], argIndexStart: Int): Ast = {
    var index = argIndexStart
    this.copy(argEdges = argEdges ++ dsts.map { dst =>
      addArgumentIndex(dst, index)
      index += 1
      Ast.neighbourValidation(src, dst, EdgeTypes.ARGUMENT)
      AstEdge(src, dst)
    })
  }
  private def addArgumentIndex(node: NewNode, argIndex: Int): Unit = node match {
    case n: NewBlock            => n.argumentIndex = argIndex
    case n: NewCall             => n.argumentIndex = argIndex
    case n: NewFieldIdentifier  => n.argumentIndex = argIndex
    case n: NewIdentifier       => n.argumentIndex = argIndex
    case n: NewMethodRef        => n.argumentIndex = argIndex
    case n: NewTypeRef          => n.argumentIndex = argIndex
    case n: NewUnknown          => n.argumentIndex = argIndex
    case n: NewControlStructure => n.argumentIndex = argIndex
    case n: NewLiteral          => n.argumentIndex = argIndex
    case n: NewReturn           => n.argumentIndex = argIndex
  }

  def withConditionEdges(src: NewNode, dsts: List[NewNode]): Ast = {
    dsts.foreach(dst => Ast.neighbourValidation(src, dst, EdgeTypes.CONDITION))
    this.copy(conditionEdges = conditionEdges ++ dsts.map(AstEdge(src, _)))
  }

  def withRefEdges(src: NewNode, dsts: List[NewNode]): Ast = {
    dsts.foreach(dst => Ast.neighbourValidation(src, dst, EdgeTypes.REF))
    this.copy(refEdges = refEdges ++ dsts.map(AstEdge(src, _)))
  }

  def withBindsEdges(src: NewNode, dsts: List[NewNode]): Ast = {
    dsts.foreach(dst => Ast.neighbourValidation(src, dst, EdgeTypes.BINDS))
    this.copy(bindsEdges = bindsEdges ++ dsts.map(AstEdge(src, _)))
  }

  def withReceiverEdges(src: NewNode, dsts: List[NewNode]): Ast = {
    dsts.foreach(dst => Ast.neighbourValidation(src, dst, EdgeTypes.RECEIVER))
    this.copy(receiverEdges = receiverEdges ++ dsts.map(AstEdge(src, _)))
  }

  /** Returns a deep copy of the sub tree rooted in `node`. If `order` is set, then the `order` and `argumentIndex`
    * fields of the new root node are set to `order`. If `replacementNode` is set, then this replaces `node` in the new
    * copy.
    */
  def subTreeCopy(node: AstNodeNew, argIndex: Int = -1, replacementNode: Option[AstNodeNew] = None): Ast = {
    val newNode = replacementNode match {
      case Some(n) => n
      case None    => node.copy
    }
    if (argIndex != -1) {
      // newNode.order = argIndex
      newNode match {
        case expr: ExpressionNew =>
          expr.argumentIndex = argIndex
        case _ =>
      }
    }

    val astChildren = edges.filter(_.src == node).map(_.dst)
    val newChildren = astChildren.map { x =>
      this.subTreeCopy(x.asInstanceOf[AstNodeNew])
    }

    val oldToNew = astChildren.zip(newChildren).map { case (old, n) => old -> n.root.get }.toMap
    def newIfExists(x: NewNode) = {
      oldToNew.getOrElse(x, x)
    }

    val newArgEdges       = argEdges.filter(_.src == node).map(x => AstEdge(newNode, newIfExists(x.dst)))
    val newConditionEdges = conditionEdges.filter(_.src == node).map(x => AstEdge(newNode, newIfExists(x.dst)))
    val newRefEdges       = refEdges.filter(_.src == node).map(x => AstEdge(newNode, newIfExists(x.dst)))
    val newBindsEdges     = bindsEdges.filter(_.src == node).map(x => AstEdge(newNode, newIfExists(x.dst)))
    val newReceiverEdges  = receiverEdges.filter(_.src == node).map(x => AstEdge(newNode, newIfExists(x.dst)))

    Ast(newNode)
      .copy(
        argEdges = newArgEdges,
        conditionEdges = newConditionEdges,
        refEdges = newRefEdges,
        bindsEdges = newBindsEdges,
        receiverEdges = newReceiverEdges
      )
      .withChildren(newChildren)
  }

}
