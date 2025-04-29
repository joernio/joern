package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.collection.mutable

object C2CpgScope {

  sealed trait ScopeElement {
    val surroundingScope: Option[ScopeElement]
    val nameToVariableNode: mutable.Map[String, (NewNode, String, String)] = mutable.HashMap.empty

    def addVariable(variableName: String, variableNode: NewNode, tpe: String, evaluationStrategy: String): Unit = {
      nameToVariableNode(variableName) = (variableNode, tpe, evaluationStrategy)
    }
  }

  case class ResolvedReference(variableNodeId: NewNode, origin: PendingReference)

  case class PendingReference(
    variableName: String,
    referenceNode: NewNode,
    tpe: String,
    var evaluationStrategy: String,
    stack: Option[ScopeElement]
  )

  enum ScopeType {
    case MethodScope, BlockScope
  }

  case class MethodScopeElement(
    methodFullName: String,
    methodName: String,
    capturingRefId: Option[NewNode],
    scopeNode: NewNode,
    surroundingScope: Option[ScopeElement]
  ) extends ScopeElement {
    def needsEnclosingScope: Boolean = {
      scopeNode.isInstanceOf[NewTypeDecl] || scopeNode.isInstanceOf[NewNamespaceBlock]
    }
  }

  private case class BlockScopeElement(scopeNode: NewNode, surroundingScope: Option[ScopeElement]) extends ScopeElement

}

class C2CpgScope {

  import C2CpgScope.*

  private val pendingReferences: mutable.Buffer[PendingReference] = mutable.ListBuffer.empty
  private var stack: Option[ScopeElement]                         = Option.empty

  def getEnclosingMethodScopeElement(scopeHead: Option[ScopeElement]): Option[MethodScopeElement] = {
    scopeHead.flatMap {
      case methodScope: MethodScopeElement => Some(methodScope)
      case blockScope: BlockScopeElement   => getEnclosingMethodScopeElement(blockScope.surroundingScope)
    }
  }

  def computeScopePath: String = {
    getEnclosingMethodScopeElement(stack) match {
      case Some(methodScope) if methodScope.methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix) =>
        val offset = NamespaceTraversal.globalNamespaceName.length + 1
        val index  = methodScope.methodFullName.indexOf(NamespaceTraversal.globalNamespaceName) + offset
        methodScope.methodFullName.substring(index).takeWhile(_ != ':')
      case Some(methodScope) if methodScope.methodName != NamespaceTraversal.globalNamespaceName =>
        methodScope.methodFullName.takeWhile(_ != ':')
      case _ => ""
    }
  }

  def lookupVariable(identifier: String): Option[(NewNode, String)] = {
    variableFromStack(stack, identifier).map { case (variableNodeId, tpe, _) => (variableNodeId, tpe) }
  }

  def variableIsInMethodScope(identifier: String): Boolean = {
    getEnclosingMethodScopeElement(stack).exists(_.nameToVariableNode.contains(identifier))
  }

  def pushNewMethodScope(
    methodFullName: String,
    methodName: String,
    scopeNode: NewNode,
    capturingRefId: Option[NewNode]
  ): Unit = {
    stack = Option(MethodScopeElement(methodFullName, methodName, capturingRefId, scopeNode, stack))
  }

  def pushNewBlockScope(scopeNode: NewNode): Unit = {
    stack = Option(BlockScopeElement(scopeNode, stack))
  }

  def popScope(): Unit = {
    stack = stack.flatMap(_.surroundingScope)
  }

  def addVariable(variableName: String, variableNode: NewNode, tpe: String, scopeType: ScopeType): Unit = {
    addVariable(stack, variableName, variableNode, tpe, EvaluationStrategies.BY_REFERENCE, scopeType)
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

  def resolve(unresolvedHandler: (NewNode, PendingReference) => NewLocal): Iterator[ResolvedReference] = {
    pendingReferences.iterator.flatMap { pendingReference =>
      tryResolve(pendingReference).orElse {
        getEnclosingMethodScopeElement(pendingReference.stack).flatMap { methodScopeElement =>
          val newVariableNode = unresolvedHandler(methodScopeElement.scopeNode, pendingReference)
          addVariable(
            pendingReference.stack,
            pendingReference.variableName,
            newVariableNode,
            pendingReference.tpe,
            pendingReference.evaluationStrategy,
            ScopeType.MethodScope
          )
          tryResolve(pendingReference)
        }
      }
    }
  }

  private def variableFromStack(
    stack: Option[ScopeElement],
    variableName: String
  ): Option[(NewNode, String, String)] = {
    stack.flatMap { stackElement =>
      if (stackElement.nameToVariableNode.contains(variableName)) { stackElement.nameToVariableNode.get(variableName) }
      else { variableFromStack(stackElement.surroundingScope, variableName) }
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
      case _                     => stack
    }
    scopeToAddTo.foreach(_.addVariable(variableName, variableNode, tpe, evaluationStrategy))
  }

  private def tryResolve(pendingReference: PendingReference): Option[ResolvedReference] = {
    variableFromStack(pendingReference.stack, pendingReference.variableName).map { case (variableNodeId, _, _) =>
      ResolvedReference(variableNodeId, pendingReference)
    }
  }

}
