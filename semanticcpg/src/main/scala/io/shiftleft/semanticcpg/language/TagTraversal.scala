package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.traversal._

class TagTraversal(val traversal: Traversal[Tag]) extends AnyVal {

  def member: Traversal[Member] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Member].sortBy(_.id)

  def method: Traversal[Method] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Method].sortBy(_.id)

  def methodReturn: Traversal[MethodReturn] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[MethodReturn].sortBy(_.id)

  def parameter: Traversal[MethodParameterIn] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[MethodParameterIn].sortBy(_.id)

  def parameterOut: Traversal[MethodParameterOut] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[MethodParameterOut].sortBy(_.id)

  def call: Traversal[Call] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Call].sortBy(_.id)

  def identifier: Traversal[Identifier] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Identifier].sortBy(_.id)

  def literal: Traversal[Literal] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Literal].sortBy(_.id)

  def local: Traversal[Local] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Local].sortBy(_.id)

  def file: Traversal[File] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[File].sortBy(_.id)

}
