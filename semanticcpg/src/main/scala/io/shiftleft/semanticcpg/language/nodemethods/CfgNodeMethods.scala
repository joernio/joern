package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.util.Try

object CfgNodeMethods {
  private val logger = LoggerFactory.getLogger(classOf[CfgNodeMethods])
}

class CfgNodeMethods(val node: CfgNode) extends AnyVal with NodeExtension {
  import CfgNodeMethods.*

  /** Successors in the CFG
    */
  def cfgNext: Iterator[CfgNode] =
    Iterator.single(node).cfgNext

  /** Maps each node in the traversal to a traversal returning its n successors.
    */
  def cfgNext(n: Int): Iterator[CfgNode] = n match {
    case 0 => Iterator.empty
    case _ => cfgNext.flatMap(x => List(x) ++ x.cfgNext(n - 1))
  }

  /** Maps each node in the traversal to a traversal returning its n predecessors.
    */
  def cfgPrev(n: Int): Iterator[CfgNode] = n match {
    case 0 => Iterator.empty
    case _ => cfgPrev.flatMap(x => List(x) ++ x.cfgPrev(n - 1))
  }

  /** Predecessors in the CFG
    */
  def cfgPrev: Iterator[CfgNode] =
    Iterator.single(node).cfgPrev

  /** Recursively determine all nodes on which this CFG node is control-dependent.
    */
  def controlledBy: Iterator[CfgNode] = {
    expandExhaustively { v =>
      v._cdgIn
    }
  }

  /** Recursively determine all nodes which this CFG node controls
    */
  def controls: Iterator[CfgNode] = {
    expandExhaustively { v =>
      v._cdgOut
    }
  }

  /** Recursively determine all nodes by which this node is dominated
    */
  def dominatedBy: Iterator[CfgNode] = {
    expandExhaustively { v =>
      v._dominateIn
    }
  }

  /** Recursively determine all nodes which are dominated by this node
    */
  def dominates: Iterator[CfgNode] = {
    expandExhaustively { v =>
      v._dominateOut
    }
  }

  /** Recursively determine all nodes by which this node is post dominated
    */
  def postDominatedBy: Iterator[CfgNode] = {
    expandExhaustively { v =>
      v._postDominateIn
    }
  }

  /** Recursively determine all nodes which are post dominated by this node
    */
  def postDominates: Iterator[CfgNode] = {
    expandExhaustively { v =>
      v._postDominateOut
    }
  }

  private def expandExhaustively(expand: CfgNode => Iterator[StoredNode]): Iterator[CfgNode] = {
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
    controllingNodes.iterator
  }

  def method: Method = node match {
    case method: Method                                                 => method
    case _: MethodParameterIn | _: MethodParameterOut | _: MethodReturn => walkUpAst(node)
    case callRepr: CallRepr if !callRepr.isInstanceOf[Call]             => walkUpAst(callRepr)
    case annotation: Annotation                                         => methodFromAnnotation(annotation)
    case annotationLiteral: AnnotationLiteral                           => methodFromAnnotation(annotationLiteral)
    case expr: Expression =>
      Try(methodViaContainsIn(expr)).recover { exception =>
        logger.info("Unable to expand to method from expr {}. Exception: {}", expr.code, exception)
        null
      }.get
    case jumpTarget: JumpTarget =>
      Try(methodViaContainsIn(jumpTarget)).recover { exception =>
        logger.info("Unable to expand to method from jumpTarget {}. Exception: {}", jumpTarget.code, exception)
        null
      }.get
  }

  private def methodViaContainsIn(node: CfgNode): Method = {
    node._containsIn.collectAll[Method].head
  }

  // Annotation nodes may not be attached to a method because of their historic CFG modeling.
  private def methodFromAnnotation(annotationNode: CfgNode): Method = {
    annotationNode.inAst.collectAll[Method].headOption.getOrElse {
      throw new RuntimeException(s"""|This method cannot be used on ANNOTATION or ANNOTATION_LITERAL nodes as they
                                     |do not necessarily belong to a METHOD. This problem is caused by annotations
                                     |invalidly implementing the CFG trait which we sadly cannot easily fix.
                                     |As workaround you can use the `methodOption` extension on the annotation
                                     |nodes, manually walk the AST or test a CONTAINS edge expansion to check if
                                     |an annotation is part of a method or not.
                                     |""".stripMargin)
    }
  }

  /** Obtain hexadecimal string representation of lineNumber field.
    *
    * Binary frontends store addresses in the lineNumber field as integers. For interoperability with other binary
    * analysis tooling, it is convenient to allow retrieving these as hex strings.
    */
  def address: Option[String] = {
    node.lineNumber.map(_.toLong.toHexString)
  }

  /** Attention: this only works for some special CfgNodes that are guaranteed to always be direct children of a
    * method... that's why it's private!
    */
  private def walkUpAst(node: CfgNode): Method = {
    node.astParent.asInstanceOf[Method]
  }

}
