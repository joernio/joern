package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.annotation.tailrec
import scala.collection.mutable

object C2CpgScope {

  abstract class ScopeElement(val scopeNode: NewNode, val surroundingScope: Option[ScopeElement]) {
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

  class MethodScopeElement(
    val methodFullName: String,
    val methodName: String,
    val capturingRefId: Option[NewNode],
    override val scopeNode: NewNode,
    override val surroundingScope: Option[ScopeElement]
  ) extends ScopeElement(scopeNode, surroundingScope) {
    def needsEnclosingScope: Boolean = {
      scopeNode.isInstanceOf[NewTypeDecl] || scopeNode.isInstanceOf[NewNamespaceBlock]
    }
  }

  class BlockScopeElement(override val scopeNode: NewNode, override val surroundingScope: Option[ScopeElement])
      extends ScopeElement(scopeNode, surroundingScope)

}

class C2CpgScope {

  import C2CpgScope.*

  private val pendingReferences: mutable.Buffer[PendingReference] = mutable.ListBuffer.empty
  private var stack: Option[ScopeElement]                         = Option.empty

  def getEnclosingMethodScopeElement(scopeHead: Option[ScopeElement]): MethodScopeElement = {
    // There are no references outside of methods. Meaning we always find a MethodScope here.
    // Outermost method scope is the synthetic <global> method scope.
    scopeHead.get match {
      case methodScopeElement: MethodScopeElement => methodScopeElement
      case blockScopeElement: BlockScopeElement   => getEnclosingMethodScopeElement(blockScopeElement.surroundingScope)
    }
  }

  def computeScopePath: String = {
    val m = getEnclosingMethodScopeElement(stack)
    if (m.methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix)) {
      val offset = NamespaceTraversal.globalNamespaceName.length + 1
      val index  = m.methodFullName.indexOf(NamespaceTraversal.globalNamespaceName) + offset
      m.methodFullName.substring(index).takeWhile(_ != ':')
    } else if (m.methodName != NamespaceTraversal.globalNamespaceName) {
      m.methodFullName.takeWhile(_ != ':')
    } else { "" }
  }

  def lookupVariable(identifier: String): Option[(NewNode, String)] = {
    variableFromStack(stack, identifier).map { case (variableNodeId, tpe, _) => (variableNodeId, tpe) }
  }

  def variableIsInMethodScope(identifier: String): Boolean = {
    getEnclosingMethodScopeElement(stack).nameToVariableNode.contains(identifier)
  }

  def pushNewMethodScope(
    methodFullName: String,
    methodName: String,
    scopeNode: NewNode,
    capturingRefId: Option[NewNode]
  ): Unit = {
    stack = Option(new MethodScopeElement(methodFullName, methodName, capturingRefId, scopeNode, stack))
  }

  def pushNewBlockScope(scopeNode: NewNode): Unit = {
    stack = Option(new BlockScopeElement(scopeNode, stack))
  }

  def popScope(): Unit = {
    stack = stack.get.surroundingScope
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
    pendingReferences.iterator.map { pendingReference =>
      val resolvedReferenceOption = tryResolve(pendingReference)
      resolvedReferenceOption.getOrElse {
        val methodScope     = getEnclosingMethodScopeElement(pendingReference.stack).scopeNode
        val newVariableNode = unresolvedHandler(methodScope, pendingReference)
        addVariable(
          pendingReference.stack,
          pendingReference.variableName,
          newVariableNode,
          pendingReference.tpe,
          pendingReference.evaluationStrategy,
          ScopeType.MethodScope
        )
        tryResolve(pendingReference).get
      }
    }
  }

  @tailrec
  private def variableFromStack(
    stack: Option[ScopeElement],
    variableName: String
  ): Option[(NewNode, String, String)] = {
    stack match {
      case Some(scope) if scope.nameToVariableNode.contains(variableName) => scope.nameToVariableNode.get(variableName)
      case Some(scope) => variableFromStack(scope.surroundingScope, variableName)
      case None        => None
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

  private def tryResolve(pendingReference: PendingReference): Option[ResolvedReference] = {
    variableFromStack(pendingReference.stack, pendingReference.variableName).map { case (variableNodeId, _, _) =>
      ResolvedReference(variableNodeId, pendingReference)
    }
  }

}
