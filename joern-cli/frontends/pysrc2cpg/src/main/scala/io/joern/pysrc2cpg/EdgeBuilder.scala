package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{
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
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

class EdgeBuilder(diffGraph: DiffGraphBuilder) {
  def astEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode, order: Int): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.AST)
    addOrder(dstNode, order)
  }

  def argumentEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode, argIndex: Int): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.ARGUMENT)
    addArgumentIndex(dstNode, argIndex)
  }

  def argumentEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode, argName: String): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.ARGUMENT)
    // We need to fill something according to the CPG spec. But the spec also says that argument
    // index is ignored if argument name is provided. So we just put -1.
    addArgumentIndex(dstNode, -1)
    addArgumentName(dstNode, argName)
  }

  def receiverEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.RECEIVER)
  }

  def conditionEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.CONDITION)
  }

  def refEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.REF)
  }

  def captureEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.CAPTURE)
  }

  def bindsEdge(dstNode: nodes.NewNode, srcNode: nodes.NewNode): Unit = {
    diffGraph.addEdge(srcNode, dstNode, EdgeTypes.BINDS)
  }

  private def addOrder(node: nodes.NewNode, order: Int): Unit = node match {
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

  private def addArgumentIndex(node: nodes.NewNode, argIndex: Int): Unit = node match {
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

  private def addArgumentName(node: nodes.NewNode, argName: String): Unit = {
    val someArgName = Some(argName)
    node match {
      case n: NewBlock            => n.argumentName = someArgName
      case n: NewCall             => n.argumentName = someArgName
      case n: NewFieldIdentifier  => n.argumentName = someArgName
      case n: NewIdentifier       => n.argumentName = someArgName
      case n: NewMethodRef        => n.argumentName = someArgName
      case n: NewTypeRef          => n.argumentName = someArgName
      case n: NewUnknown          => n.argumentName = someArgName
      case n: NewControlStructure => n.argumentName = someArgName
      case n: NewLiteral          => n.argumentName = someArgName
      case n: NewReturn           => n.argumentName = someArgName
    }
  }
}
