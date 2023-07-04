package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.traversal._
import scala.reflect.ClassTag

class TagTraversal(val traversal: Traversal[Tag]) extends AnyVal {

  def member: Traversal[Member]                   = tagged[Member]
  def method: Traversal[Method]                   = tagged[Method]
  def methodReturn: Traversal[MethodReturn]       = tagged[MethodReturn]
  def parameter: Traversal[MethodParameterIn]     = tagged[MethodParameterIn]
  def parameterOut: Traversal[MethodParameterOut] = tagged[MethodParameterOut]
  def call: Traversal[Call]                       = tagged[Call]
  def identifier: Traversal[Identifier]           = tagged[Identifier]
  def literal: Traversal[Literal]                 = tagged[Literal]
  def local: Traversal[Local]                     = tagged[Local]
  def file: Traversal[File]                       = tagged[File]

  private def tagged[A <: StoredNode: ClassTag]: Traversal[A] =
    traversal._taggedByIn.collectAll[A].sortBy(_.id).iterator
}
