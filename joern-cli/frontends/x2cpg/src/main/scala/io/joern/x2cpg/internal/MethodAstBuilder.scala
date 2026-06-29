package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, PropertyDefaults}

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
