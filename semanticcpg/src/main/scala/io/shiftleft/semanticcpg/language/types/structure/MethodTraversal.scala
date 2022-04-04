package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb._
import overflowdb.traversal.help.Doc
import overflowdb.traversal.{Traversal, help, toElementTraversal, toNodeTraversal}

/** A method, function, or procedure
  */
@help.Traversal(elementType = classOf[Method])
class MethodTraversal(val iterableOnce: IterableOnce[Method]) extends AnyVal {

  /** Traverse to annotations of method
    */
  def annotation: Traversal[nodes.Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** All control structures of this method
    */
  @Doc(info = "Control structures (source frontends only)")
  def controlStructure: Traversal[ControlStructure] =
    traversal.ast.isControlStructure

  /** Shorthand to traverse to control structures where condition matches `regex`
    */
  def controlStructure(regex: String): Traversal[ControlStructure] =
    traversal.ast.isControlStructure.code(regex)

  @Doc(info = "All try blocks (`ControlStructure` nodes)")
  def tryBlock: Traversal[ControlStructure] =
    controlStructure.isTry

  @Doc(info = "All if blocks (`ControlStructure` nodes)")
  def ifBlock: Traversal[ControlStructure] =
    controlStructure.isIf

  @Doc(info = "All else blocks (`ControlStructure` nodes)")
  def elseBlock: Traversal[ControlStructure] =
    controlStructure.isElse

  @Doc(info = "All switch blocks (`ControlStructure` nodes)")
  def switchBlock: Traversal[ControlStructure] =
    controlStructure.isSwitch

  @Doc(info = "All do blocks (`ControlStructure` nodes)")
  def doBlock: Traversal[ControlStructure] =
    controlStructure.isDo

  @Doc(info = "All for blocks (`ControlStructure` nodes)")
  def forBlock: Traversal[ControlStructure] =
    controlStructure.isFor

  @Doc(info = "All while blocks (`ControlStructure` nodes)")
  def whileBlock: Traversal[ControlStructure] =
    controlStructure.isWhile

  @Doc(info = "All gotos (`ControlStructure` nodes)")
  def goto: Traversal[ControlStructure] =
    controlStructure.isGoto

  @Doc(info = "All breaks (`ControlStructure` nodes)")
  def break: Traversal[ControlStructure] =
    controlStructure.isBreak

  @Doc(info = "All continues (`ControlStructure` nodes)")
  def continue: Traversal[ControlStructure] =
    controlStructure.isContinue

  @Doc(info = "All throws (`ControlStructure` nodes)")
  def throws: Traversal[ControlStructure] =
    controlStructure.isThrow

  /** The type declaration associated with this method, e.g., the class it is defined in.
    */
  @Doc(info = "Type this method is defined in")
  def definingTypeDecl: Traversal[TypeDecl] =
    traversal
      .repeat(_.in(EdgeTypes.AST))(_.until(_.hasLabel(NodeTypes.TYPE_DECL)))
      .cast[TypeDecl]

  /** The method in which this method is defined
    */
  @Doc(info = "Method this method is defined in")
  def definingMethod: Traversal[Method] =
    traversal
      .repeat(_.in(EdgeTypes.AST))(_.until(_.hasLabel(NodeTypes.METHOD)))
      .cast[Method]

  /** Traverse only to methods that are stubs, e.g., their code is not available or the method body is empty.
    */
  def isStub: Traversal[Method] =
    traversal.where(_.not(_.out(EdgeTypes.CFG).not(_.hasLabel(NodeTypes.METHOD_RETURN))))

  /** Traverse only to methods that are not stubs.
    */
  def isNotStub: Traversal[Method] =
    traversal.where(_.out(EdgeTypes.CFG).not(_.hasLabel(NodeTypes.METHOD_RETURN)))

  /** Traverse only to methods that accept variadic arguments.
    */
  def isVariadic: Traversal[Method] = {
    traversal.filter(_.isVariadic)
  }

  /** Traverse to external methods, that is, methods not present but only referenced in the CPG.
    */
  @Doc(info = "External methods (called, but no body available)")
  def external: Traversal[Method] = {
    traversal.has(Properties.IS_EXTERNAL -> true)
  }

  /** Traverse to internal methods, that is, methods for which code is included in this CPG.
    */
  @Doc(info = "Internal methods, i.e., a body is available")
  def internal: Traversal[Method] =
    traversal.has(Properties.IS_EXTERNAL -> false)

  /** Traverse to the methods local variables
    */
  @Doc(info = "Local variables declared in the method")
  def local: Traversal[Local] =
    traversal.block.ast.isLocal

  @Doc(info = "Top level expressions (\"Statements\")")
  def topLevelExpressions: Traversal[Expression] =
    traversal
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.BLOCK)
      .out(EdgeTypes.AST)
      .not(_.hasLabel(NodeTypes.LOCAL))
      .cast[Expression]

  @Doc(info = "Control flow graph nodes")
  def cfgNode: Traversal[CfgNode] =
    traversal.flatMap(_.cfgNode)

  /** Traverse to last expression in CFG.
    */
  @Doc(info = "Last control flow graph node")
  def cfgLast: Traversal[CfgNode] =
    traversal.methodReturn.cfgLast

  /** Traverse to method body (alias for `block`) */
  @Doc(info = "Alias for `block`")
  def body: Traversal[Block] =
    traversal.block

  /** Traverse to namespace */
  @Doc(info = "Namespace this method is declared in")
  def namespace: Traversal[Namespace] = {
    traversal.choose(_.astParentType) {
      case NamespaceBlock.Label =>
        // some language frontends don't have a TYPE_DECL for a METHOD
        _.astParent.collectAll[NamespaceBlock].namespace
      case _ =>
        // other language frontends always embed their method in a TYPE_DECL
        _.definingTypeDecl.namespace
    }
  }

  def numberOfLines: Traversal[Int] = traversal.map(_.numberOfLines)

  private def traversal = Traversal.from(iterableOnce)
}
