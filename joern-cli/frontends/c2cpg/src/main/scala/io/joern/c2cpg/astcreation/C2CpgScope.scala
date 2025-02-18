package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef

import scala.collection.mutable

object C2CpgScope {

  def getEnclosingMethodScopeElement(scopeHead: Option[ScopeElement]): MethodScopeElement = {
    // There are no references outside of methods. Meaning we always find a MethodScope here.
    new ScopeElementIterator(scopeHead)
      .collectFirst { case methodScopeElement: MethodScopeElement => methodScopeElement }
      .getOrElse(throw new RuntimeException("Cannot find method scope."))
  }

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

  case class ResolvedReference(variableNodeId: NewNode, origin: PendingReference)

  case class PendingReference(
    variableName: String,
    referenceNode: NewNode,
    tpe: String,
    var evaluationStrategy: String,
    stack: Option[ScopeElement]
  ) {
    def tryResolve(): Option[ResolvedReference] = {
      var foundVariableOption = Option.empty[(NewNode, String, String)]
      val stackIterator       = new ScopeElementIterator(stack)

      while (stackIterator.hasNext && foundVariableOption.isEmpty) {
        val scopeElement = stackIterator.next()
        foundVariableOption = scopeElement.nameToVariableNode.get(variableName)
      }

      foundVariableOption.map { case (variableNodeId, _, _) =>
        ResolvedReference(variableNodeId, this)
      }
    }
  }

  enum ScopeType {
    case MethodScope, BlockScope
  }

  abstract class ScopeElement(val name: String, val scopeNode: NewNode, val surroundingScope: Option[ScopeElement]) {
    var subScopeCounter: Int                                               = 0
    val nameToVariableNode: mutable.Map[String, (NewNode, String, String)] = mutable.HashMap.empty

    def addVariable(variableName: String, variableNode: NewNode, tpe: String, evaluationStrategy: String): Unit =
      nameToVariableNode(variableName) = (variableNode, tpe, evaluationStrategy)
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

}

class C2CpgScope {

  import C2CpgScope.*

  private val pendingReferences: mutable.Buffer[PendingReference] = mutable.ListBuffer.empty[PendingReference]

  private var stack = Option.empty[ScopeElement]

  def lookupVariable(identifier: String): Option[(NewNode, String)] = {
    var foundVariableOption = Option.empty[(NewNode, String, String)]
    val stackIterator       = new ScopeElementIterator(stack)

    while (stackIterator.hasNext && foundVariableOption.isEmpty) {
      val scopeElement = stackIterator.next()
      foundVariableOption = scopeElement.nameToVariableNode.get(identifier)
    }

    foundVariableOption.map { case (variableNodeId, tpe, _) =>
      (variableNodeId, tpe)
    }
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
    pendingReferences prepend PendingReference(variableName, referenceNode, tpe, evaluationStrategy, stack)
  }

  def updateVariableReference(referenceNode: NewNode, evaluationStrategy: String): Unit = {
    pendingReferences.find(_.referenceNode == referenceNode).foreach(r => r.evaluationStrategy = evaluationStrategy)
  }

  def resolve(unresolvedHandler: (NewNode, String, String) => NewNode): Iterator[ResolvedReference] = {
    pendingReferences.iterator.map { pendingReference =>
      val resolvedReferenceOption = pendingReference.tryResolve()
      resolvedReferenceOption.getOrElse {
        val methodScope     = C2CpgScope.getEnclosingMethodScopeElement(pendingReference.stack).scopeNode
        val newVariableNode = unresolvedHandler(methodScope, pendingReference.variableName, pendingReference.tpe)
        addVariable(
          pendingReference.stack,
          pendingReference.variableName,
          newVariableNode,
          pendingReference.tpe,
          pendingReference.evaluationStrategy,
          C2CpgScope.ScopeType.MethodScope
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
      case C2CpgScope.ScopeType.MethodScope => C2CpgScope.getEnclosingMethodScopeElement(stack)
      case _                                => stack.get
    }
    scopeToAddTo.addVariable(variableName, variableNode, tpe, evaluationStrategy)
  }

}
