package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import io.shiftleft.codepropertygraph.generated.v2.accessors.Lang.*
import io.shiftleft.codepropertygraph.generated.v2.neighboraccessors.Lang.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodMethods(val method: Method) extends AnyVal with NodeExtension with HasLocation {

  /** Traverse to annotations of method
    */
  def annotation: Iterator[Annotation] =
    method._annotationViaAstOut

  def local: Iterator[Local] =
    method._blockViaContainsOut.local

  /** All control structures of this method
    */
  def controlStructure: Iterator[ControlStructure] = {
    ???
    // TODO bring back: need AstNodeMethods first
//    method.ast.isControlStructure
  }

  def numberOfLines: Int = {
    if (method.lineNumber.isDefined && method.lineNumberEnd.isDefined) {
      method.lineNumberEnd.get - method.lineNumber.get + 1
    } else {
      0
    }
  }

  def isVariadic: Boolean = {
    method.parameter.exists(_.isVariadic)
  }

  def cfgNode: Iterator[CfgNode] =
    method._containsOut.collectAll[CfgNode]

  /** List of CFG nodes in reverse post order
    */
  def reversePostOrder: Iterator[CfgNode] = {
    def expand(x: CfgNode) = {
      // TODO: bring back: need CfgNodeMethods first
      //      x.cfgNext.iterator
      ???
    }

    NodeOrdering.reverseNodeList(NodeOrdering.postOrderNumbering(method, expand).toList).iterator
  }

  /** List of CFG nodes in post order
    */
  def postOrder: Iterator[CfgNode] = {
    def expand(x: CfgNode) = {
      // TODO: bring back: need CfgNodeMethods first
      //      x.cfgNext.iterator
      ???
    }
    NodeOrdering.nodeList(NodeOrdering.postOrderNumbering(method, expand).toList).iterator
  }

  /** The type declaration associated with this method, e.g., the class it is defined in.
    */
  def definingTypeDecl: Option[TypeDecl] =
    // TODO: bring back: need MethodTraversal.scala first
//    Iterator.single(method).definingTypeDecl.headOption
    ???

  /** The type declaration associated with this method, e.g., the class it is defined in. Alias for 'definingTypeDecl'
    */
  def typeDecl: Option[TypeDecl] = definingTypeDecl

  /** Traverse to method body (alias for `block`) */
  def body: Block =
    method.block

  override def location: NewLocation = {
    LocationCreator(method, method.name, method.label, method.lineNumber, method)
  }
}