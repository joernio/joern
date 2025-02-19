package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import scala.collection.mutable

object C2CpgScope {

  def getEnclosingMethodScopeElement(scopeHead: Option[ScopeElement]): MethodScopeElement = {
    // There are no references outside of methods. Meaning we always find a MethodScope here.
    // Outermost method scope is the synthetic <global> method scope.
    new ScopeElementIterator(scopeHead).collectFirst { case elem: MethodScopeElement => elem }.get
  }

  private def variableFromStack(
    stack: Option[ScopeElement],
    variableName: String
  ): Option[(NewNode, String, String)] = {
    new ScopeElementIterator(stack)
      .find(_.nameToVariableNode.contains(variableName))
      .flatMap(_.nameToVariableNode.get(variableName))
  }

  abstract class ScopeElement(val name: String, val scopeNode: NewNode, val surroundingScope: Option[ScopeElement]) {
    val nameToVariableNode: mutable.Map[String, (NewNode, String, String)] = mutable.HashMap.empty
    var subScopeCounter: Int                                               = 0

    def addVariable(variableName: String, variableNode: NewNode, tpe: String, evaluationStrategy: String): Unit =
      nameToVariableNode(variableName) = (variableNode, tpe, evaluationStrategy)
  }

  case class ResolvedReference(variableNodeId: NewNode, origin: PendingReference)

  case class PendingReference(
    variableName: String,
    referenceNode: NewNode,
    tpe: String,
    var evaluationStrategy: String,
    stack: Option[ScopeElement]
  ) {
    def tryResolve(): Option[ResolvedReference] = {
      variableFromStack(stack, variableName).map { case (variableNodeId, _, _) =>
        ResolvedReference(variableNodeId, this)
      }
    }
  }

  enum ScopeType {
    case MethodScope, BlockScope
  }

  class MethodScopeElement(
    val methodFullName: String,
    val capturingRefId: Option[NewNode],
    override val name: String,
    override val scopeNode: NewNode,
    override val surroundingScope: Option[ScopeElement]
  ) extends ScopeElement(name, scopeNode, surroundingScope) {
    def needsEnclosingScope: Boolean = {
      scopeNode.isInstanceOf[NewTypeDecl] || scopeNode.isInstanceOf[NewNamespaceBlock]
    }
  }

  class BlockScopeElement(
    override val name: String,
    override val scopeNode: NewNode,
    override val surroundingScope: Option[ScopeElement]
  ) extends ScopeElement(name, scopeNode, surroundingScope)

  private class ScopeElementIterator(start: Option[ScopeElement]) extends Iterator[ScopeElement] {
    private var currentScopeElement = start

    override def hasNext: Boolean = {
      currentScopeElement.isDefined
    }

    override def next(): ScopeElement = {
      val result = currentScopeElement.get
      currentScopeElement = result.surroundingScope
      result
    }
  }

}

class C2CpgScope {

  import C2CpgScope.*

  private val pendingReferences: mutable.Buffer[PendingReference] = mutable.ListBuffer.empty[PendingReference]

  private var stack = Option.empty[ScopeElement]

  def lookupVariable(identifier: String): Option[(NewNode, String)] = {
    variableFromStack(stack, identifier).map { case (variableNodeId, tpe, _) => (variableNodeId, tpe) }
  }

  def pushNewMethodScope(
    methodFullName: String,
    name: String,
    scopeNode: NewNode,
    capturingRefId: Option[NewNode]
  ): Unit = {
    stack = Option(new MethodScopeElement(methodFullName, capturingRefId, name, scopeNode, surroundingScope = stack))
  }

  def pushNewBlockScope(scopeNode: NewNode): Unit = {
    stack match {
      case Some(stackTop) =>
        stack = Option(new BlockScopeElement(stackTop.subScopeCounter.toString, scopeNode, surroundingScope = stack))
        stackTop.subScopeCounter += 1
      case None =>
        stack = Option(new BlockScopeElement("0", scopeNode, surroundingScope = stack))
    }
  }

  def popScope(): Unit = {
    stack = stack.get.surroundingScope
  }

  def addVariable(variableName: String, variableNode: NewNode, tpe: String, scopeType: ScopeType): Unit = {
    addVariable(stack, variableName, variableNode, tpe, EvaluationStrategies.BY_VALUE, scopeType)
  }

  def addVariableReference(
    variableName: String,
    referenceNode: NewNode,
    tpe: String,
    evaluationStrategy: String
  ): Unit = {
    pendingReferences.prepend(PendingReference(variableName, referenceNode, tpe, evaluationStrategy, stack))
  }

  def updateVariableReference(referenceNode: NewNode, evaluationStrategy: String): Unit = {
    pendingReferences.find(_.referenceNode == referenceNode).foreach(r => r.evaluationStrategy = evaluationStrategy)
  }

  def resolve(unresolvedHandler: (NewNode, String, String) => NewNode): Iterator[ResolvedReference] = {
    pendingReferences.iterator.map { pendingReference =>
      val resolvedReferenceOption = pendingReference.tryResolve()
      resolvedReferenceOption.getOrElse {
        val methodScope     = getEnclosingMethodScopeElement(pendingReference.stack).scopeNode
        val newVariableNode = unresolvedHandler(methodScope, pendingReference.variableName, pendingReference.tpe)
        addVariable(
          pendingReference.stack,
          pendingReference.variableName,
          newVariableNode,
          pendingReference.tpe,
          pendingReference.evaluationStrategy,
          ScopeType.MethodScope
        )
        pendingReference.tryResolve().get
      }
    }
  }

  private def addVariable(
    stack: Option[ScopeElement],
    variableName: String,
    variableNode: NewNode,
    tpe: String,
    evaluationStrategy: String,
    scopeType: ScopeType
  ): Unit = {
    val scopeToAddTo = scopeType match {
      case ScopeType.MethodScope => getEnclosingMethodScopeElement(stack)
      case _                     => stack.get
    }
    scopeToAddTo.addVariable(variableName, variableNode, tpe, evaluationStrategy)
  }

}
