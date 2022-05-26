package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Properties}
import overflowdb._
import overflowdb.traversal.help
import overflowdb.traversal.help.Doc
import overflowdb.traversal.{Traversal, TraversalSource, jIteratortoTraversal, toElementTraversal}

@help.TraversalSource
class NodeTypeStarters(cpg: Cpg) extends TraversalSource(cpg.graph) {

  /** Traverse to all nodes.
    */
  @Doc(info = "All nodes of the graph")
  override def all: Traversal[StoredNode] =
    cpg.graph.nodes.cast[StoredNode]

  /** Traverse to all annotations
    */
  def annotation: Traversal[Annotation] =
    cpg.graph.nodes(NodeTypes.ANNOTATION).cast[Annotation]

  /** Traverse to all arguments passed to methods
    */
  @Doc(info = "All arguments (actual parameters)")
  def argument: Traversal[Expression] =
    call.argument

  /** Shorthand for `cpg.argument.code(code)`
    */
  def argument(code: String): Traversal[Expression] =
    argument.code(code)

  @Doc(info = "All breaks (`ControlStructure` nodes)")
  def break: Traversal[ControlStructure] =
    controlStructure.isBreak

  /** Traverse to all call sites
    */
  @Doc(info = "All call sites")
  def call: Traversal[Call] =
    cpg.graph.nodes(NodeTypes.CALL).cast[Call]

  /** Shorthand for `cpg.call.name(name)`
    */
  def call(name: String): Traversal[Call] =
    call.name(name)

  /** Traverse to all comments in source-based CPGs.
    */
  @Doc(info = "All comments in source-based CPGs")
  def comment: Traversal[Comment] =
    cpg.graph.nodes(NodeTypes.COMMENT).cast[Comment]

  /** Shorthand for `cpg.comment.code(code)`
    */
  def comment(code: String): Traversal[Comment] =
    comment.has(Properties.CODE -> code)

  @Doc(info = "All control structures (source-based frontends)")
  def controlStructure: Traversal[ControlStructure] =
    cpg.graph.nodes(NodeTypes.CONTROL_STRUCTURE).cast[ControlStructure]

  @Doc(info = "All continues (`ControlStructure` nodes)")
  def continue: Traversal[ControlStructure] =
    controlStructure.isContinue

  @Doc(info = "All do blocks (`ControlStructure` nodes)")
  def doBlock: Traversal[ControlStructure] =
    controlStructure.isDo

  @Doc(info = "All else blocks (`ControlStructure` nodes)")
  def elseBlock: Traversal[ControlStructure] =
    controlStructure.isElse

  @Doc(info = "All throws (`ControlStructure` nodes)")
  def throws: Traversal[ControlStructure] =
    controlStructure.isThrow

  /** Traverse to all source files
    */
  @Doc(info = "All source files")
  def file: Traversal[File] =
    cpg.graph.nodes(NodeTypes.FILE).cast[File]

  /** Shorthand for `cpg.file.name(name)`
    */
  def file(name: String): Traversal[File] =
    file.name(name)

  @Doc(info = "All for blocks (`ControlStructure` nodes)")
  def forBlock: Traversal[ControlStructure] =
    controlStructure.isFor

  @Doc(info = "All gotos (`ControlStructure` nodes)")
  def goto: Traversal[ControlStructure] =
    controlStructure.isGoto

  /** Traverse to all identifiers, e.g., occurrences of local variables or class members in method bodies.
    */
  @Doc(info = "All identifier usages")
  def identifier: Traversal[Identifier] =
    cpg.graph.nodes(NodeTypes.IDENTIFIER).cast[Identifier]

  /** Shorthand for `cpg.identifier.name(name)`
    */
  def identifier(name: String): Traversal[Identifier] =
    identifier.name(name)

  @Doc(info = "All if blocks (`ControlStructure` nodes)")
  def ifBlock: Traversal[ControlStructure] =
    controlStructure.isIf

  /** Traverse to all jump targets
    */
  @Doc(info = "All jump targets, i.e., labels")
  def jumpTarget: Traversal[JumpTarget] =
    cpg.graph.nodes(NodeTypes.JUMP_TARGET).cast[JumpTarget]

  /** Traverse to all local variable declarations
    */
  @Doc(info = "All local variables")
  def local: Traversal[Local] =
    cpg.graph.nodes(NodeTypes.LOCAL).cast[Local]

  /** Shorthand for `cpg.local.name`
    */
  def local(name: String): Traversal[Local] =
    local.name(name)

  /** Traverse to all literals (constant strings and numbers provided directly in the code).
    */
  @Doc(info = "All literals, e.g., numbers or strings")
  def literal: Traversal[Literal] =
    cpg.graph.nodes(NodeTypes.LITERAL).cast[Literal]

  /** Shorthand for `cpg.literal.code(code)`
    */
  def literal(code: String): Traversal[Literal] =
    literal.code(code)

  /** Traverse to all methods
    */
  @Doc(info = "All methods")
  def method: Traversal[Method] =
    cpg.graph.nodes(NodeTypes.METHOD).cast[Method]

  /** Shorthand for `cpg.method.name(fullName)`
    */
  @Doc(info = "All methods with given name")
  def method(name: String): Traversal[Method] =
    method.name(name)

  /** Traverse to all formal return parameters
    */
  @Doc(info = "All formal return parameters")
  def methodReturn: Traversal[MethodReturn] =
    cpg.graph.nodes(NodeTypes.METHOD_RETURN).cast[MethodReturn]

  /** Traverse to all class members
    */
  @Doc(info = "All members of complex types (e.g., classes/structures)")
  def member: Traversal[Member] =
    cpg.graph.nodes(NodeTypes.MEMBER).cast[Member]

  /** Shorthand for `cpg.member.name(name)`
    */
  def member(name: String): Traversal[Member] =
    member.name(name)

  /** Traverse to all meta data entries
    */
  @Doc(info = "Meta data blocks for graph")
  def metaData: Traversal[MetaData] =
    cpg.graph.nodes(NodeTypes.META_DATA).cast[MetaData]

  /** Traverse to all method references
    */
  @Doc(info = "All method references")
  def methodRef: Traversal[MethodRef] =
    cpg.graph.nodes(NodeTypes.METHOD_REF).cast[MethodRef]

  /** Shorthand for `cpg.methodRef.filter(_.referencedMethod.name(name))`
    */
  def methodRef(name: String): Traversal[MethodRef] =
    methodRef.where(_.referencedMethod.name(name))

  /** Traverse to all namespaces, e.g., packages in Java.
    */
  @Doc(info = "All namespaces")
  def namespace: Traversal[Namespace] =
    cpg.graph.nodes(NodeTypes.NAMESPACE).cast[Namespace]

  /** Shorthand for `cpg.namespace.name(name)`
    */
  def namespace(name: String): Traversal[Namespace] =
    namespace.name(name)

  /** Traverse to all namespace blocks, e.g., packages in Java.
    */
  def namespaceBlock: Traversal[NamespaceBlock] =
    cpg.graph.nodes(NodeTypes.NAMESPACE_BLOCK).cast[NamespaceBlock]

  /** Shorthand for `cpg.namespaceBlock.name(name)`
    */
  def namespaceBlock(name: String): Traversal[NamespaceBlock] =
    namespaceBlock.name(name)

  /** Traverse to all input parameters
    */
  @Doc(info = "All parameters")
  def parameter: Traversal[MethodParameterIn] =
    cpg.graph.nodes(NodeTypes.METHOD_PARAMETER_IN).cast[MethodParameterIn]

  /** Shorthand for `cpg.parameter.name(name)`
    */
  def parameter(name: String): Traversal[MethodParameterIn] =
    parameter.name(name)

  /** Traverse to all return expressions
    */
  @Doc(info = "All actual return parameters")
  def ret: Traversal[Return] =
    cpg.graph.nodes(NodeTypes.RETURN).cast[Return]

  /** Shorthand for `returns.code(code)`
    */
  def ret(code: String): Traversal[Return] =
    ret.code(code)

  @Doc(info = "All switch blocks (`ControlStructure` nodes)")
  def switchBlock: Traversal[ControlStructure] =
    controlStructure.isSwitch

  @Doc(info = "All try blocks (`ControlStructure` nodes)")
  def tryBlock: Traversal[ControlStructure] =
    controlStructure.isTry

  /** Traverse to all types, e.g., Set<String>
    */
  @Doc(info = "All used types")
  def typ: Traversal[Type] =
    cpg.graph.nodes(NodeTypes.TYPE).cast[Type]

  /** Shorthand for `cpg.typ.name(name)`
    */
  @Doc(info = "All used types with given name")
  def typ(name: String): Traversal[Type] =
    typ.fullName(name)

  /** Traverse to all declarations, e.g., Set<T>
    */
  @Doc(info = "All declarations of types")
  def typeDecl: Traversal[TypeDecl] =
    cpg.graph.nodes(NodeTypes.TYPE_DECL).cast[TypeDecl]

  /** Shorthand for cpg.typeDecl.name(name)
    */
  def typeDecl(name: String): Traversal[TypeDecl] =
    typeDecl.name(name)

  /** Traverse to all tags
    */
  @Doc(info = "All tags")
  def tag: Traversal[Tag] =
    cpg.graph.nodes(NodeTypes.TAG).cast[Tag]

  @Doc(info = "All tags with given name")
  def tag(name: String): Traversal[Tag] =
    tag.name(name)

  /** Traverse to all type references
    */
  @Doc(info = "All type references")
  def typeRef: Traversal[TypeRef] =
    cpg.graph.nodes(NodeTypes.TYPE_REF).cast[TypeRef]

  @Doc(info = "All while blocks (`ControlStructure` nodes)")
  def whileBlock: Traversal[ControlStructure] =
    controlStructure.isWhile

}
