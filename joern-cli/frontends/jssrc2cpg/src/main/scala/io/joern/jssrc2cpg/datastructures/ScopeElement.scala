package io.joern.jssrc2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.collection.mutable

/** A single element of a scope stack.
  */
abstract class ScopeElement(val name: String, val scopeNode: NewNode, val surroundingScope: Option[ScopeElement]) {
  var subScopeCounter: Int                             = 0
  val nameToVariableNode: mutable.Map[String, NewNode] = mutable.HashMap.empty

  def addVariable(variableName: String, variableNode: NewNode): Unit =
    nameToVariableNode(variableName) = variableNode
}

class MethodScopeElement(
  val methodFullName: String,
  val capturingRefId: Option[NewNode],
  override val name: String,
  override val scopeNode: NewNode,
  override val surroundingScope: Option[ScopeElement]
) extends ScopeElement(name, scopeNode, surroundingScope)

class BlockScopeElement(
  override val name: String,
  override val scopeNode: NewNode,
  override val surroundingScope: Option[ScopeElement]
) extends ScopeElement(name, scopeNode, surroundingScope)
