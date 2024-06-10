package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Properties}
import overflowdb._
import overflowdb.traversal.help
import overflowdb.traversal.help.Doc
import overflowdb.traversal.{InitialTraversal, TraversalSource}

import scala.jdk.CollectionConverters.IteratorHasAsScala

@help.TraversalSource
class NodeTypeStarters(cpg: Cpg) extends TraversalSource(cpg.graph) {

  /** Traverse to all nodes.
    */
  @Doc(info = "All nodes of the graph")
  override def all: Traversal[StoredNode] =
    cpg.graph.nodes.asScala.cast[StoredNode]

  /** Traverse to all annotations
    */
  def annotation: Traversal[Annotation] =
    InitialTraversal.from[Annotation](cpg.graph, NodeTypes.ANNOTATION)

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
    InitialTraversal.from[Call](cpg.graph, NodeTypes.CALL)

  /** Shorthand for `cpg.call.name(name)`
    */
  def call(name: String): Traversal[Call] =
    call.name(name)

  /** Traverse to all comments in source-based CPGs.
    */
  @Doc(info = "All comments in source-based CPGs")
  def comment: Traversal[Comment] =
    InitialTraversal.from[Comment](cpg.graph, NodeTypes.COMMENT)

  /** Shorthand for `cpg.comment.code(code)`
    */
  def comment(code: String): Traversal[Comment] =
    comment.has(Properties.CODE -> code)

  /** Traverse to all config files
    */
  @Doc(info = "All config files")
  def configFile: Traversal[ConfigFile] =
    InitialTraversal.from[ConfigFile](cpg.graph, NodeTypes.CONFIG_FILE)

  /** Shorthand for `cpg.configFile.name(name)`
    */
  def configFile(name: String): Traversal[ConfigFile] =
    configFile.name(name)

  /** Traverse to all dependencies
    */
  @Doc(info = "All dependencies")
  def dependency: Traversal[Dependency] =
    InitialTraversal.from[Dependency](cpg.graph, NodeTypes.DEPENDENCY)

  /** Shorthand for `cpg.dependency.name(name)`
    */
  def dependency(name: String): Traversal[Dependency] =
    dependency.name(name)

  @Doc(info = "All control structures (source-based frontends)")
  def controlStructure: Traversal[ControlStructure] =
    InitialTraversal.from[ControlStructure](cpg.graph, NodeTypes.CONTROL_STRUCTURE)

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
    InitialTraversal.from[File](cpg.graph, NodeTypes.FILE)

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
    InitialTraversal.from[Identifier](cpg.graph, NodeTypes.IDENTIFIER)

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
    InitialTraversal.from[JumpTarget](cpg.graph, NodeTypes.JUMP_TARGET)

  /** Traverse to all local variable declarations
    */
  @Doc(info = "All local variables")
  def local: Traversal[Local] =
    InitialTraversal.from[Local](cpg.graph, NodeTypes.LOCAL)

  /** Shorthand for `cpg.local.name`
    */
  def local(name: String): Traversal[Local] =
    local.name(name)

  /** Traverse to all literals (constant strings and numbers provided directly in the code).
    */
  @Doc(info = "All literals, e.g., numbers or strings")
  def literal: Traversal[Literal] =
    InitialTraversal.from[Literal](cpg.graph, NodeTypes.LITERAL)

  /** Shorthand for `cpg.literal.code(code)`
    */
  def literal(code: String): Traversal[Literal] =
    literal.code(code)

  /** Traverse to all methods
    */
  @Doc(info = "All methods")
  def method: Traversal[Method] =
    InitialTraversal.from[Method](cpg.graph, NodeTypes.METHOD)

  /** Shorthand for `cpg.method.name(name)`
    */
  @Doc(info = "All methods with a name that matches the given pattern")
  def method(namePattern: String): Traversal[Method] =
    method.name(namePattern)

  /** Traverse to all formal return parameters
    */
  @Doc(info = "All formal return parameters")
  def methodReturn: Traversal[MethodReturn] =
    InitialTraversal.from[MethodReturn](cpg.graph, NodeTypes.METHOD_RETURN)

  /** Traverse to all class members
    */
  @Doc(info = "All members of complex types (e.g., classes/structures)")
  def member: Traversal[Member] =
    InitialTraversal.from[Member](cpg.graph, NodeTypes.MEMBER)

  /** Shorthand for `cpg.member.name(name)`
    */
  def member(name: String): Traversal[Member] =
    member.name(name)

  /** Traverse to all meta data entries
    */
  @Doc(info = "Meta data blocks for graph")
  def metaData: Traversal[MetaData] =
    InitialTraversal.from[MetaData](cpg.graph, NodeTypes.META_DATA)

  /** Traverse to all method references
    */
  @Doc(info = "All method references")
  def methodRef: Traversal[MethodRef] =
    InitialTraversal.from[MethodRef](cpg.graph, NodeTypes.METHOD_REF)

  /** Shorthand for `cpg.methodRef.filter(_.referencedMethod.name(name))`
    */
  def methodRef(name: String): Traversal[MethodRef] =
    methodRef.where(_.referencedMethod.name(name))

  /** Traverse to all namespaces, e.g., packages in Java.
    */
  @Doc(info = "All namespaces")
  def namespace: Traversal[Namespace] =
    InitialTraversal.from[Namespace](cpg.graph, NodeTypes.NAMESPACE)

  /** Shorthand for `cpg.namespace.name(name)`
    */
  def namespace(name: String): Traversal[Namespace] =
    namespace.name(name)

  /** Traverse to all namespace blocks, e.g., packages in Java.
    */
  def namespaceBlock: Traversal[NamespaceBlock] =
    InitialTraversal.from[NamespaceBlock](cpg.graph, NodeTypes.NAMESPACE_BLOCK)

  /** Shorthand for `cpg.namespaceBlock.name(name)`
    */
  def namespaceBlock(name: String): Traversal[NamespaceBlock] =
    namespaceBlock.name(name)

  /** Traverse to all input parameters
    */
  @Doc(info = "All parameters")
  def parameter: Traversal[MethodParameterIn] =
    InitialTraversal.from[MethodParameterIn](cpg.graph, NodeTypes.METHOD_PARAMETER_IN)

  /** Shorthand for `cpg.parameter.name(name)`
    */
  def parameter(name: String): Traversal[MethodParameterIn] =
    parameter.name(name)

  /** Traverse to all return expressions
    */
  @Doc(info = "All actual return parameters")
  def ret: Traversal[Return] =
    InitialTraversal.from[Return](cpg.graph, NodeTypes.RETURN)

  /** Shorthand for `returns.code(code)`
    */
  def ret(code: String): Traversal[Return] =
    ret.code(code)

  @Doc(info = "All imports")
  def imports: Traversal[Import] =
    InitialTraversal.from[Import](cpg.graph, NodeTypes.IMPORT)

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
    InitialTraversal.from[Type](cpg.graph, NodeTypes.TYPE)

  /** Shorthand for `cpg.typ.name(name)`
    */
  @Doc(info = "All used types with given name")
  def typ(name: String): Traversal[Type] =
    typ.name(name)

  /** Traverse to all declarations, e.g., Set<T>
    */
  @Doc(info = "All declarations of types")
  def typeDecl: Traversal[TypeDecl] =
    InitialTraversal.from[TypeDecl](cpg.graph, NodeTypes.TYPE_DECL)

  /** Shorthand for cpg.typeDecl.name(name)
    */
  def typeDecl(name: String): Traversal[TypeDecl] =
    typeDecl.name(name)

  /** Traverse to all tags
    */
  @Doc(info = "All tags")
  def tag: Traversal[Tag] =
    InitialTraversal.from[Tag](cpg.graph, NodeTypes.TAG)

  @Doc(info = "All tags with given name")
  def tag(name: String): Traversal[Tag] =
    tag.name(name)

  /** Traverse to all template DOM nodes
    */
  @Doc(info = "All template DOM nodes")
  def templateDom: Traversal[TemplateDom] =
    InitialTraversal.from[TemplateDom](cpg.graph, NodeTypes.TEMPLATE_DOM)

  /** Traverse to all type references
    */
  @Doc(info = "All type references")
  def typeRef: Traversal[TypeRef] =
    InitialTraversal.from[TypeRef](cpg.graph, NodeTypes.TYPE_REF)

  @Doc(info = "All while blocks (`ControlStructure` nodes)")
  def whileBlock: Traversal[ControlStructure] =
    controlStructure.isWhile

}
