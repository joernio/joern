package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.traversal._

class TagTraversal(val traversal: Traversal[Tag]) extends AnyVal {

  def member: Traversal[Member] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Member]

  def method: Traversal[Method] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Method]

  def methodReturn: Traversal[MethodReturn] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[MethodReturn]

  def parameter: Traversal[MethodParameterIn] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[MethodParameterIn]

  def parameterOut: Traversal[MethodParameterOut] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[MethodParameterOut]

  def call: Traversal[Call] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Call]

  def identifier: Traversal[Identifier] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Identifier]

  def literal: Traversal[Literal] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Literal]

  def local: Traversal[Local] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[Local]

  def file: Traversal[File] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[File]

}
