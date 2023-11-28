package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.reflect.ClassTag

class TagTraversal(val traversal: Iterator[Tag]) extends AnyVal {

  def member: Iterator[Member]                   = tagged[Member]
  def method: Iterator[Method]                   = tagged[Method]
  def methodReturn: Iterator[MethodReturn]       = tagged[MethodReturn]
  def parameter: Iterator[MethodParameterIn]     = tagged[MethodParameterIn]
  def parameterOut: Iterator[MethodParameterOut] = tagged[MethodParameterOut]
  def call: Iterator[Call]                       = tagged[Call]
  def identifier: Iterator[Identifier]           = tagged[Identifier]
  def literal: Iterator[Literal]                 = tagged[Literal]
  def local: Iterator[Local]                     = tagged[Local]
  def file: Iterator[File]                       = tagged[File]

  private def tagged[A <: StoredNode: ClassTag]: Iterator[A] =
    traversal._taggedByIn.collectAll[A].sortBy(_.id).iterator
}
