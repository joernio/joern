package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{
  Annotation,
  Block,
  CfgNode,
  ControlStructure,
  Local,
  Method,
  NewLocation,
  TypeDecl
}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{Traversal, jIteratortoTraversal}

class MethodMethods(val method: Method) extends AnyVal with NodeExtension with HasLocation {

  /** Traverse to annotations of method
    */
  def annotation: Traversal[Annotation] =
    method._annotationViaAstOut

  def local: Traversal[Local] =
    method._blockViaContainsOut.flatMap(_._localViaAstOut)

  /** All control structures of this method
    */
  def controlStructure: Traversal[ControlStructure] =
    method.ast.isControlStructure

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

  def cfgNode: Traversal[CfgNode] =
    method._containsOut.collectAll[CfgNode]

  /** List of CFG nodes in reverse post order
    */
  def reversePostOrder: Traversal[CfgNode] = {
    def expand(x: CfgNode) = { x.cfgNext.iterator }
    Traversal.from(NodeOrdering.reverseNodeList(NodeOrdering.postOrderNumbering(method, expand).toList))
  }

  /** List of CFG nodes in post order
    */
  def postOrder: Traversal[CfgNode] = {
    def expand(x: CfgNode) = { x.cfgNext.iterator }
    Traversal.from(NodeOrdering.nodeList(NodeOrdering.postOrderNumbering(method, expand).toList))
  }

  /** The type declaration associated with this method, e.g., the class it is defined in.
    */
  def definingTypeDecl: Traversal[TypeDecl] =
    Traversal.fromSingle(method).definingTypeDecl

  /** The type declaration associated with this method, e.g., the class it is defined in. Alias for 'definingTypeDecl'
    */
  def typeDecl: Traversal[TypeDecl] = definingTypeDecl

  /** Traverse to method body (alias for `block`) */
  // TODO MP: return a `Block` rather than `Traversal[Block]` - it's guaranteed to be there and exactly one...
  def body: Traversal[Block] =
    Traversal.fromSingle(method.block)

  override def location: NewLocation = {
    LocationCreator(method, method.name, method.label, method.lineNumber, method)
  }
}
