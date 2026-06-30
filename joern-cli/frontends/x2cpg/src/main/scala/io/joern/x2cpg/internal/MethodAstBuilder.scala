package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, PropertyDefaults}

/** Mixin that provides helpers for building method ASTs.
  *
  * Covers full method bodies, method stubs, static initialisers, and return statements.
  *
  * Mixed into [[io.joern.x2cpg.AstCreatorBase]] via the `internal` package; not intended for direct use outside
  * `x2cpg`.
  */
private[x2cpg] trait MethodAstBuilder[Node, NodeProcessor] {
  this: AstCreatorBase[Node, NodeProcessor] =>

  /** Creates an AST that represents an entire method, including its content. */
  def methodAst(
    method: NewMethod,
    parameters: Seq[Ast],
    body: Ast,
    methodReturn: NewMethodReturn,
    modifiers: Seq[NewModifier] = Nil
  ): Ast =
    methodAstWithAnnotations(method, parameters, body, methodReturn, modifiers, annotations = Nil)

  /** Creates an AST that represents an entire method, including its content and with support for both method and
    * parameter annotations.
    */
  def methodAstWithAnnotations(
    method: NewMethod,
    parameters: Seq[Ast],
    body: Ast,
    methodReturn: NewMethodReturn,
    modifiers: Seq[NewModifier] = Nil,
    annotations: Seq[Ast] = Nil
  ): Ast =
    Ast(method)
      .withChildren(parameters)
      .withChild(body)
      .withChildren(modifiers.map(Ast(_)))
      .withChildren(annotations)
      .withChild(Ast(methodReturn))

  /** Creates an AST that represents a method stub, containing information about the method, its parameters, and the
    * return type.
    */
  def methodStubAst(
    method: NewMethod,
    parameters: Seq[Ast],
    methodReturn: NewMethodReturn,
    modifiers: Seq[NewModifier] = Nil
  ): Ast =
    Ast(method)
      .withChildren(parameters)
      .withChild(Ast(NewBlock().typeFullName(Defines.Any)))
      .withChildren(modifiers.map(Ast(_)))
      .withChild(Ast(methodReturn))

  /** Creates an AST for a static initialiser method (e.g. a class-level `static { … }` block or module-level init).
    *
    * The method is given the [[io.joern.x2cpg.Defines.StaticInitMethodName]] name and a `STATIC` modifier. Its body
    * wraps `initAsts` in a block.
    *
    * @param node
    *   the source AST node that triggered the static init (used for position)
    * @param initAsts
    *   ordered list of statement ASTs that form the initialiser body
    * @param fullName
    *   fully-qualified name of the synthesised method
    * @param signature
    *   optional method signature; defaults to [[io.shiftleft.codepropertygraph.generated.PropertyDefaults.Signature]]
    * @param returnType
    *   fully-qualified return type name
    * @param fileName
    *   optional source file name; defaults to
    *   [[io.shiftleft.codepropertygraph.generated.PropertyDefaults.Filename]]
    */
  def staticInitMethodAst(
    node: Node,
    initAsts: List[Ast],
    fullName: String,
    signature: Option[String],
    returnType: String,
    fileName: Option[String] = None
  ): Ast = {
    val methodNode_ = methodNode(
      node,
      Defines.StaticInitMethodName,
      fullName,
      signature.getOrElse(PropertyDefaults.Signature),
      fileName.getOrElse(PropertyDefaults.Filename)
    )
    val staticModifier = NewModifier().modifierType(ModifierTypes.STATIC)
    val body           = blockAst(NewBlock().typeFullName(Defines.Any), initAsts)
    val methodReturn   = methodReturnNode(node, returnType)
    methodAst(methodNode_, Nil, body, methodReturn, List(staticModifier))
  }

  /** For a given return node and arguments, create an AST that represents the return instruction. The main purpose of
    * this method is to automatically assign the correct argument indices.
    */
  def returnAst(returnNode: NewReturn, arguments: Seq[Ast] = List()): Ast = {
    setArgumentIndices(arguments)
    Ast(returnNode)
      .withChildren(arguments)
      .withArgEdges(returnNode, arguments.flatMap(_.root))
  }
}
