package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{
  CpgNode,
  NewBlock,
  NewCall,
  NewControlStructure,
  NewFieldIdentifier,
  NewFile,
  NewIdentifier,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewMethodReturn,
  NewModifier,
  NewNamespaceBlock,
  NewReturn,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.DiffGraph

class EdgeBuilder(diffGraph: DiffGraph.Builder) {
  def astEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode, order: Int): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.AST)
    addOrder(dstNode, order)
  }

  def argumentEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode, argIndex: Int): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.ARGUMENT)
    addArgumentIndex(dstNode, argIndex)
  }

  def receiverEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.RECEIVER)
  }

  def conditionEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.CONDITION)
  }

  private def addOrder(node: CpgNode, order: Int): Unit = node match {
    case n: NewTypeDecl          => n.order = order
    case n: NewBlock             => n.order = order
    case n: NewCall              => n.order = order
    case n: NewFieldIdentifier   => n.order = order
    case n: NewFile              => n.order = order
    case n: NewIdentifier        => n.order = order
    case n: NewLocal             => n.order = order
    case n: NewMethod            => n.order = order
    case n: NewMethodParameterIn => n.order = order
    case n: NewMethodRef         => n.order = order
    case n: NewNamespaceBlock    => n.order = order
    case n: NewTypeRef           => n.order = order
    case n: NewUnknown           => n.order = order
    case n: NewModifier          => n.order = order
    case n: NewMethodReturn      => n.order = order
    case n: NewMember            => n.order = order
    case n: NewControlStructure  => n.order = order
    case n: NewLiteral           => n.order = order
    case n: NewReturn            => n.order = order
    case n: NewJumpTarget        => n.order = order
  }

  private def addArgumentIndex(node: CpgNode, argIndex: Int): Unit = node match {
    case n: NewBlock            => n.argumentIndex = argIndex
    case n: NewCall             => n.argumentIndex = argIndex
    case n: NewFieldIdentifier  => n.argumentIndex = argIndex
    case n: NewIdentifier       => n.argumentIndex = argIndex
    case n: NewMethodRef        => n.argumentIndex = argIndex
    case n: NewTypeRef          => n.argumentIndex = argIndex
    case n: NewUnknown          => n.argumentIndex = argIndex
    case n: NewControlStructure => n.argumentIndex = argIndex
    case n: NewLiteral          => n.argumentIndex = argIndex
    case n: NewReturn           => n.argumentIndex = argIndex
  }
}
