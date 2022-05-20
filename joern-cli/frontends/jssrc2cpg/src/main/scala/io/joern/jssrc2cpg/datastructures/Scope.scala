package io.joern.jssrc2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.collection.mutable

/** Handles the scope stack for tracking identifier to variable relation.
  */
class Scope {

  private val pendingReferences: mutable.Buffer[PendingReference] =
    mutable.ListBuffer.empty[PendingReference]

  private var stack = Option.empty[ScopeElement]

  def getScopeHead: Option[ScopeElement] = stack

  def getPendingReferences: List[PendingReference] = pendingReferences.toList

  def isEmpty: Boolean = stack.isEmpty

  def pushNewMethodScope(
    methodFullName: String,
    name: String,
    scopeNode: NewNode,
    capturingRefId: Option[NewNode]
  ): Unit =
    stack = Some(new MethodScopeElement(methodFullName, capturingRefId, name, scopeNode, surroundingScope = stack))

  def pushNewBlockScope(scopeNode: NewNode): Unit = {
    peek match {
      case Some(stackTop) =>
        stack = Some(new BlockScopeElement(stackTop.subScopeCounter.toString, scopeNode, surroundingScope = stack))
        stackTop.subScopeCounter += 1
      case None =>
        stack = Some(new BlockScopeElement("0", scopeNode, surroundingScope = stack))
    }
  }

  private def peek: Option[ScopeElement] = {
    stack
  }

  def popScope(): Unit = {
    stack = stack.get.surroundingScope
  }

  def addVariable(variableName: String, variableNode: NewNode, scopeType: ScopeType): Unit = {
    addVariable(stack, variableName, variableNode, scopeType)
  }

  def addVariableReference(variableName: String, referenceNode: NewNode): Unit = {
    pendingReferences prepend PendingReference(variableName, referenceNode, stack)
  }

  def resolve(unresolvedHandler: (NewNode, String) => (NewNode, ScopeType)): Iterator[ResolvedReference] = {
    pendingReferences.iterator.map { pendingReference =>
      val resolvedReferenceOption = pendingReference.tryResolve()

      resolvedReferenceOption.getOrElse {
        val methodScopeNodeId = Scope.getEnclosingMethodScope(pendingReference.stack)
        val (newVariableNode, scopeType) =
          unresolvedHandler(methodScopeNodeId, pendingReference.variableName)
        addVariable(pendingReference.stack, pendingReference.variableName, newVariableNode, scopeType)
        pendingReference.tryResolve().get
      }
    }

  }

  private def addVariable(
    stack: Option[ScopeElement],
    variableName: String,
    variableNode: NewNode,
    scopeType: ScopeType
  ): Unit = {
    val scopeToAddTo = scopeType match {
      case MethodScope =>
        new ScopeElementIterator(stack).find(_.isInstanceOf[MethodScopeElement]).get
      case _ => stack.get
    }
    scopeToAddTo.addVariable(variableName, variableNode)
  }

}

object Scope {
  def getEnclosingMethodScope(scopeHead: Option[ScopeElement]): NewNode = {
    new ScopeElementIterator(scopeHead)
      .collectFirst { case methodScopeElement: MethodScopeElement =>
        methodScopeElement.scopeNode
      }
      .getOrElse(throw new RuntimeException("Cannot find method scope."))
    // There are no references outside of methods. Meaning we always find a MethodScope here.
  }
}

class ScopeElementIterator(start: Option[ScopeElement]) extends Iterator[ScopeElement] {
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
