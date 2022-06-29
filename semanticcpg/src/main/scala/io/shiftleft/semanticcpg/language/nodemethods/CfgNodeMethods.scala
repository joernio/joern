package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.Implicits.JavaIteratorDeco
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import scala.jdk.CollectionConverters._

class CfgNodeMethods(val node: CfgNode) extends AnyVal with NodeExtension {

  /** Textual representation of CFG node
    */
  def repr: String =
    node match {
      case method: Method                             => method.name
      case methodReturn: MethodReturn                 => methodReturn.code
      case expr: Expression                           => expr.code
      case call: CallRepr if !call.isInstanceOf[Call] => call.code
    }

  /** Successors in the CFG
    */
  def cfgNext: Traversal[CfgNode] = {
    Traversal.fromSingle(node).cfgNext
  }

  /** Maps each node in the traversal to a traversal returning its n successors.
    */
  def cfgNext(n: Int): Traversal[CfgNode] = n match {
    case 0 => Traversal()
    case _ => cfgNext.flatMap(x => List(x) ++ x.cfgNext(n - 1))
  }

  /** Maps each node in the traversal to a traversal returning its n predecessors.
    */
  def cfgPrev(n: Int): Traversal[CfgNode] = n match {
    case 0 => Traversal()
    case _ => cfgPrev.flatMap(x => List(x) ++ x.cfgPrev(n - 1))
  }

  /** Predecessors in the CFG
    */
  def cfgPrev: Traversal[CfgNode] = {
    Traversal.fromSingle(node).cfgPrev
  }

  /** Recursively determine all nodes on which this CFG node is control-dependent.
    */
  def controlledBy: Traversal[CfgNode] = {
    expandExhaustively { v =>
      v._cdgIn.asScala
    }
  }

  /** Recursively determine all nodes which this CFG node controls
    */
  def controls: Traversal[CfgNode] = {
    expandExhaustively { v =>
      v._cdgOut.asScala
    }
  }

  /** Recursively determine all nodes by which this node is dominated
    */
  def dominatedBy: Traversal[CfgNode] = {
    expandExhaustively { v =>
      v._dominateIn.asScala
    }
  }

  /** Recursively determine all nodes which are dominated by this node
    */
  def dominates: Traversal[CfgNode] = {
    expandExhaustively { v =>
      v._dominateOut.asScala
    }
  }

  /** Recursively determine all nodes by which this node is post dominated
    */
  def postDominatedBy: Traversal[CfgNode] = {
    expandExhaustively { v =>
      v._postDominateIn.asScala
    }
  }

  /** Recursively determine all nodes which are post dominated by this node
    */
  def postDominates: Traversal[CfgNode] = {
    expandExhaustively { v =>
      v._postDominateOut.asScala
    }
  }

  /** Using the post dominator tree, will determine if this node passes through the included set of nodes and filter it
    * in.
    * @param included
    *   the nodes this node must pass through.
    * @return
    *   the traversal of this node if it passes through the included set.
    */
  def passes(included: Set[CfgNode]): Traversal[CfgNode] =
    node.filter(_.postDominatedBy.exists(included.contains))

  /** Using the post dominator tree, will determine if this node passes through the excluded set of nodes and filter it
    * out.
    * @param excluded
    *   the nodes this node must not pass through.
    * @return
    *   the traversal of this node if it does not pass through the excluded set.
    */
  def passesNot(excluded: Set[CfgNode]): Traversal[CfgNode] =
    node.filterNot(_.postDominatedBy.exists(excluded.contains))

  private def expandExhaustively(expand: CfgNode => Iterator[StoredNode]): Traversal[CfgNode] = {
    var controllingNodes = List.empty[CfgNode]
    var visited          = Set.empty + node
    var worklist         = node :: Nil

    while (worklist.nonEmpty) {
      val vertex = worklist.head
      worklist = worklist.tail

      expand(vertex).foreach { case controllingNode: CfgNode =>
        if (!visited.contains(controllingNode)) {
          visited += controllingNode
          controllingNodes = controllingNode :: controllingNodes
          worklist = controllingNode :: worklist
        }
      }
    }
    Traversal.from(controllingNodes)
  }

  def method: Method = node match {
    case node: Method => node
    case _: MethodParameterIn | _: MethodParameterOut | _: MethodReturn =>
      walkUpAst(node)
    case _: CallRepr if !node.isInstanceOf[Call] => walkUpAst(node)
    case _: Expression | _: JumpTarget           => walkUpContains(node)
  }

  /** Obtain hexadecimal string representation of lineNumber field.
    *
    * Binary frontends store addresses in the lineNumber field as integers. For interoperability with other binary
    * analysis tooling, it is convenient to allow retrieving these as hex strings.
    */
  def address: Option[String] = {
    node.lineNumber.map(_.toLong.toHexString)
  }

  private def walkUpAst(node: CfgNode): Method =
    node._astIn.onlyChecked.asInstanceOf[Method]

  private def walkUpContains(node: StoredNode): Method =
    node._containsIn.onlyChecked match {
      case method: Method => method
      case _: TypeDecl    =>
        // TODO - there are csharp CPGs that have typedecls here, which is invalid.
        null
    }

}
