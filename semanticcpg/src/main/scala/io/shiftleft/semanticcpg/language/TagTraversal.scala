package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import overflowdb.traversal._

class TagTraversal(val traversal: Traversal[Tag]) extends AnyVal {

  def member: Traversal[Member] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.MEMBER)
      .sortBy(_.id)
      .cast[Member]

  def method: Traversal[Method] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.METHOD)
      .sortBy(_.id)
      .cast[Method]

  def methodReturn: Traversal[MethodReturn] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.METHOD_RETURN)
      .sortBy(_.id)
      .cast[MethodReturn]

  def parameter: Traversal[MethodParameterIn] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.METHOD_PARAMETER_IN)
      .sortBy(_.id)
      .cast[MethodParameterIn]

  def parameterOut: Traversal[MethodParameterOut] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.METHOD_PARAMETER_OUT)
      .sortBy(_.id)
      .cast[MethodParameterOut]

  def call: Traversal[Call] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.CALL)
      .sortBy(_.id)
      .cast[Call]

  def identifier: Traversal[Identifier] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.IDENTIFIER)
      .sortBy(_.id)
      .cast[Identifier]

  def literal: Traversal[Literal] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.LITERAL)
      .sortBy(_.id)
      .cast[Literal]

  def local: Traversal[Local] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.LOCAL)
      .sortBy(_.id)
      .cast[Local]

  def file: Traversal[File] =
    traversal
      .in(EdgeTypes.TAGGED_BY)
      .hasLabel(NodeTypes.FILE)
      .sortBy(_.id)
      .cast[File]

}
