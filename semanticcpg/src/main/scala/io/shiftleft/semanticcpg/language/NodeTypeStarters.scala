package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.v2.CpgGeneratedNodeStarters
import io.shiftleft.codepropertygraph.generated.v2.nodes.*
//import io.shiftleft.codepropertygraph.generated.v2.{NodeTypes, Properties}

import scala.jdk.CollectionConverters.IteratorHasAsScala

// @help.TraversalSource
class NodeTypeStarters(cpg: Cpg) {

  // TODO make CpgGeneratedNodeStarters an extension method, then drop this
  private implicit def toCpgGeneratedNodeStarters(cpg: Cpg): CpgGeneratedNodeStarters =
    new CpgGeneratedNodeStarters(cpg)

  /** Traverse to all nodes.
    */
  // @Doc(info = "All nodes of the graph")
  override def all: Iterator[StoredNode] =
    // TODO implement in flatgraph first
    ???
//    cpg.graph.nodes.asScala.cast[StoredNode]

  /** Traverse to all annotations
    */
  def annotation: Iterator[Annotation] =
    cpg.annotation

  /** Traverse to all arguments passed to methods
    */
  // @Doc(info = "All arguments (actual parameters)")
  def argument: Iterator[Expression] =
    call.argument

  /** Shorthand for `cpg.argument.code(code)`
    */
  def argument(code: String): Iterator[Expression] =
    argument.code(code)

  // @Doc(info = "All breaks (`ControlStructure` nodes)")
  def break: Iterator[ControlStructure] =
    controlStructure.isBreak

  /** Traverse to all call sites
    */
  // @Doc(info = "All call sites")
  def call: Iterator[Call] =
    cpg.call

  /** Shorthand for `cpg.call.name(name)`
    */
  def call(name: String): Iterator[Call] =
    call.name(name)

  /** Traverse to all comments in source-based CPGs.
    */
  // @Doc(info = "All comments in source-based CPGs")
  def comment: Iterator[Comment] =
    cpg.comment

  /** Shorthand for `cpg.comment.code(code)`
    */
  def comment(code: String): Iterator[Comment] =
    comment.has(Properties.CODE -> code)

  /** Traverse to all config files
    */
  // @Doc(info = "All config files")
  def configFile: Iterator[ConfigFile] =
    cpg.configFile

  /** Shorthand for `cpg.configFile.name(name)`
    */
  def configFile(name: String): Iterator[ConfigFile] =
    configFile.name(name)

  /** Traverse to all dependencies
    */
  // @Doc(info = "All dependencies")
  def dependency: Iterator[Dependency] =
    cpg.dependency

  /** Shorthand for `cpg.dependency.name(name)`
    */
  def dependency(name: String): Iterator[Dependency] =
    dependency.name(name)

  // @Doc(info = "All control structures (source-based frontends)")
  def controlStructure: Iterator[ControlStructure] =
    cpg.controlStructure

  // @Doc(info = "All continues (`ControlStructure` nodes)")
  def continue: Iterator[ControlStructure] =
    controlStructure.isContinue

  // @Doc(info = "All do blocks (`ControlStructure` nodes)")
  def doBlock: Iterator[ControlStructure] =
    controlStructure.isDo

  // @Doc(info = "All else blocks (`ControlStructure` nodes)")
  def elseBlock: Iterator[ControlStructure] =
    controlStructure.isElse

  // @Doc(info = "All throws (`ControlStructure` nodes)")
  def throws: Iterator[ControlStructure] =
    controlStructure.isThrow

  /** Traverse to all source files
    */
  // @Doc(info = "All source files")
  def file: Iterator[File] =
    cpg.file

  /** Shorthand for `cpg.file.name(name)`
    */
  def file(name: String): Iterator[File] =
    file.name(name)

  // @Doc(info = "All for blocks (`ControlStructure` nodes)")
  def forBlock: Iterator[ControlStructure] =
    controlStructure.isFor

  // @Doc(info = "All gotos (`ControlStructure` nodes)")
  def goto: Iterator[ControlStructure] =
    controlStructure.isGoto

  /** Traverse to all identifiers, e.g., occurrences of local variables or class members in method bodies.
    */
  // @Doc(info = "All identifier usages")
  def identifier: Iterator[Identifier] =
    cpg.identifier

  /** Shorthand for `cpg.identifier.name(name)`
    */
  def identifier(name: String): Iterator[Identifier] =
    identifier.name(name)

  // @Doc(info = "All if blocks (`ControlStructure` nodes)")
  def ifBlock: Iterator[ControlStructure] =
    controlStructure.isIf

  /** Traverse to all jump targets
    */
  // @Doc(info = "All jump targets, i.e., labels")
  def jumpTarget: Iterator[JumpTarget] =
    cpg.jumpTarget

  /** Traverse to all local variable declarations
    */
  // @Doc(info = "All local variables")
  def local: Iterator[Local] =
    cpg.local

  /** Shorthand for `cpg.local.name`
    */
  def local(name: String): Iterator[Local] =
    local.name(name)

  /** Traverse to all literals (constant strings and numbers provided directly in the code).
    */
  // @Doc(info = "All literals, e.g., numbers or strings")
  def literal: Iterator[Literal] =
    cpg.literal

  /** Shorthand for `cpg.literal.code(code)`
    */
  def literal(code: String): Iterator[Literal] =
    literal.code(code)

  /** Traverse to all methods
    */
  // @Doc(info = "All methods")
  def method: Iterator[Method] =
    cpg.method

  /** Shorthand for `cpg.method.name(name)`
    */
  // @Doc(info = "All methods with a name that matches the given pattern")
  def method(namePattern: String): Iterator[Method] =
    method.name(namePattern)

  /** Traverse to all formal return parameters
    */
  // @Doc(info = "All formal return parameters")
  def methodReturn: Iterator[MethodReturn] =
    cpg.methodReturn

  /** Traverse to all class members
    */
  // @Doc(info = "All members of complex types (e.g., classes/structures)")
  def member: Iterator[Member] =
    cpg.member

  /** Shorthand for `cpg.member.name(name)`
    */
  def member(name: String): Iterator[Member] =
    member.name(name)

  /** Traverse to all meta data entries
    */
  // @Doc(info = "Meta data blocks for graph")
  def metaData: Iterator[MetaData] =
    cpg.metaData

  /** Traverse to all method references
    */
  // @Doc(info = "All method references")
  def methodRef: Iterator[MethodRef] =
    cpg.methodRef

  /** Shorthand for `cpg.methodRef.filter(_.referencedMethod.name(name))`
    */
  def methodRef(name: String): Iterator[MethodRef] =
    methodRef.where(_.referencedMethod.name(name))

  /** Traverse to all namespaces, e.g., packages in Java.
    */
  // @Doc(info = "All namespaces")
  def namespace: Iterator[Namespace] =
    cpg.namespace

  /** Shorthand for `cpg.namespace.name(name)`
    */
  def namespace(name: String): Iterator[Namespace] =
    namespace.name(name)

  /** Traverse to all namespace blocks, e.g., packages in Java.
    */
  def namespaceBlock: Iterator[NamespaceBlock] =
    cpg.namespaceBlock

  /** Shorthand for `cpg.namespaceBlock.name(name)`
    */
  def namespaceBlock(name: String): Iterator[NamespaceBlock] =
    namespaceBlock.name(name)

  /** Traverse to all input parameters
    */
  // @Doc(info = "All parameters")
  def parameter: Iterator[MethodParameterIn] =
    cpg.methodParameterIn

  /** Shorthand for `cpg.parameter.name(name)`
    */
  def parameter(name: String): Iterator[MethodParameterIn] =
    parameter.name(name)

  /** Traverse to all return expressions
    */
  // @Doc(info = "All actual return parameters")
  def ret: Iterator[Return] =
    cpg.ret

  /** Shorthand for `returns.code(code)`
    */
  def ret(code: String): Iterator[Return] =
    ret.code(code)

  // @Doc(info = "All imports")
  def imports: Iterator[Import] =
    cpg.imports

  // @Doc(info = "All switch blocks (`ControlStructure` nodes)")
  def switchBlock: Iterator[ControlStructure] =
    controlStructure.isSwitch

  // @Doc(info = "All try blocks (`ControlStructure` nodes)")
  def tryBlock: Iterator[ControlStructure] =
    controlStructure.isTry

  /** Traverse to all types, e.g., Set<String>
    */
  // @Doc(info = "All used types")
  def typ: Iterator[Type] =
    cpg.typ

  /** Shorthand for `cpg.typ.name(name)`
    */
  // @Doc(info = "All used types with given name")
  def typ(name: String): Iterator[Type] =
    typ.name(name)

  /** Traverse to all declarations, e.g., Set<T>
    */
  // @Doc(info = "All declarations of types")
  def typeDecl: Iterator[TypeDecl] =
    cpg.typeDecl

  /** Shorthand for cpg.typeDecl.name(name)
    */
  def typeDecl(name: String): Iterator[TypeDecl] =
    typeDecl.name(name)

  /** Traverse to all tags
    */
  // @Doc(info = "All tags")
  def tag: Iterator[Tag] =
    cpg.tag

  // @Doc(info = "All tags with given name")
  def tag(name: String): Iterator[Tag] =
    tag.name(name)

  /** Traverse to all template DOM nodes
    */
  // @Doc(info = "All template DOM nodes")
  def templateDom: Iterator[TemplateDom] =
    cpg.templateDom

  /** Traverse to all type references
    */
  // @Doc(info = "All type references")
  def typeRef: Iterator[TypeRef] =
    cpg.typeRef

  // @Doc(info = "All while blocks (`ControlStructure` nodes)")
  def whileBlock: Iterator[ControlStructure] =
    controlStructure.isWhile

}
