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

  def isEmpty: Boolean = stack.isEmpty

  def pushNewMethodScope(
    methodFullName: String,
    name: String,
    scopeNode: NewNode,
    capturingRefId: Option[NewNode]
  ): Unit =
    stack = Option(new MethodScopeElement(methodFullName, capturingRefId, name, scopeNode, surroundingScope = stack))

  def pushNewBlockScope(scopeNode: NewNode): Unit = {
    peek match {
      case Some(stackTop) =>
        stack = Option(new BlockScopeElement(stackTop.subScopeCounter.toString, scopeNode, surroundingScope = stack))
        stackTop.subScopeCounter += 1
      case None =>
        stack = Option(new BlockScopeElement("0", scopeNode, surroundingScope = stack))
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
        val methodScopeNode = Scope.getEnclosingMethodScopeNode(pendingReference.stack)
        val (newVariableNode, scopeType) =
          unresolvedHandler(methodScopeNode, pendingReference.variableName)
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
      case MethodScope => Scope.getEnclosingMethodScopeElement(stack)
      case _           => stack.get
    }
    scopeToAddTo.addVariable(variableName, variableNode)
  }

}

object Scope {
  private def getEnclosingMethodScopeNode(scopeHead: Option[ScopeElement]): NewNode =
    getEnclosingMethodScopeElement(scopeHead).scopeNode

  def getEnclosingMethodScopeElement(scopeHead: Option[ScopeElement]): MethodScopeElement = {
    // There are no references outside of methods. Meaning we always find a MethodScope here.
    new ScopeElementIterator(scopeHead)
      .collectFirst { case methodScopeElement: MethodScopeElement => methodScopeElement }
      .getOrElse(throw new RuntimeException("Cannot find method scope."))
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
