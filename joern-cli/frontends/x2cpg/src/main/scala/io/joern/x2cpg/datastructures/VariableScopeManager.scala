package io.joern.x2cpg.datastructures

import io.joern.x2cpg.AstNodeBuilder.{closureBindingNode, localNodeWithExplicitPositionInfo}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, EdgeTypes, EvaluationStrategies}

import scala.collection.mutable

/** Object containing definitions and utilities for managing variable scopes during CPG conversion.
  */
object VariableScopeManager {

  /** Trait representing a generic scope element. Each scope element can have a surrounding scope and a map of variable
    * names to their nodes.
    */
  protected sealed trait ScopeElement {
    val surroundingScope: Option[ScopeElement]
    val nameToVariableNode: mutable.Map[String, (NewNode, String, String)] = mutable.HashMap.empty

    /** Adds a variable to the current scope.
      *
      * @param variableName
      *   The name of the variable.
      * @param variableNode
      *   The node representing the variable.
      * @param tpe
      *   The type of the variable.
      * @param evaluationStrategy
      *   The evaluation strategy for the variable.
      */
    def addVariable(variableName: String, variableNode: NewNode, tpe: String, evaluationStrategy: String): Unit = {
      nameToVariableNode(variableName) = (variableNode, tpe, evaluationStrategy)
    }
  }

  /** Enumeration representing the types of scopes.
    */
  enum ScopeType {
    case MethodScope, BlockScope
  }

  /** Case class representing a method scope element.
    *
    * @param methodFullName
    *   The fully qualified name of the method.
    * @param methodName
    *   The name of the method.
    * @param capturingRefNode
    *   An optional referenced node for capturing variables.
    * @param scopeNode
    *   The node representing the scope.
    * @param surroundingScope
    *   The surrounding scope, if any.
    */
  protected case class MethodScopeElement(
    methodFullName: String,
    methodName: String,
    capturingRefNode: Option[NewNode],
    scopeNode: NewNode,
    surroundingScope: Option[ScopeElement]
  ) extends ScopeElement {

    /** Determines if the method scope needs an enclosing scope.
      *
      * @return
      *   True if the scope node is a type declaration or namespace block, false otherwise.
      */
    def needsEnclosingScope: Boolean = {
      scopeNode.isInstanceOf[NewTypeDecl] || scopeNode.isInstanceOf[NewNamespaceBlock]
    }
  }

  /** Case class representing a resolved reference to a variable.
    *
    * @param variableNode
    *   The node of the resolved variable.
    * @param origin
    *   The original pending reference.
    */
  private case class ResolvedReference(variableNode: NewNode, origin: PendingReference)

  /** Case class representing a pending reference to a variable.
    *
    * @param variableName
    *   The name of the variable being referenced.
    * @param referenceNode
    *   The node representing the reference.
    * @param tpe
    *   The type of the variable.
    * @param evaluationStrategy
    *   The evaluation strategy for the reference.
    * @param stack
    *   The scope stack at the time of the reference.
    */
  private case class PendingReference(
    variableName: String,
    referenceNode: NewNode,
    tpe: String,
    var evaluationStrategy: String,
    stack: Option[ScopeElement]
  )

  /** Case class representing a block scope element.
    *
    * @param scopeNode
    *   The node representing the block scope.
    * @param surroundingScope
    *   The surrounding scope, if any.
    */
  private case class BlockScopeElement(scopeNode: NewNode, surroundingScope: Option[ScopeElement]) extends ScopeElement

}

/** Class for managing variable scopes during CPG conversion. Provides methods for managing scope stacks, adding
  * variables, resolving references, and creating links between variables and their references.
  */
class VariableScopeManager {

  import VariableScopeManager.*

  /** Buffer to store pending references to variables that need to be resolved later. */
  private val pendingReferences: mutable.Buffer[PendingReference] = mutable.ListBuffer.empty

  /** The current scope stack, represented as an optional `ScopeElement`. */
  protected var stack: Option[ScopeElement] = Option.empty

  /** Computes the scope path by concatenating the names of all enclosing method scopes.
    *
    * @return
    *   A string representing the scope path.
    */
  def computeScopePath: String = {
    getAllEnclosingMethodScopeElements(stack).reverse.map(_.methodName).mkString(":")
  }

  /** Looks up a variable by its identifier in the current scope stack.
    *
    * @param identifier
    *   The name of the variable.
    * @return
    *   An optional tuple containing the variable node and its type.
    */
  def lookupVariable(identifier: String): Option[(NewNode, String)] = {
    variableFromStack(stack, identifier).map { case (variableNodeId, tpe, _) => (variableNodeId, tpe) }
  }

  /** Checks if a variable is in the current method scope.
    *
    * @param identifier
    *   The name of the variable.
    * @return
    *   True if the variable is in the method scope, false otherwise.
    */
  def variableIsInMethodScope(identifier: String): Boolean = {
    getEnclosingMethodScopeElement(stack).exists(_.nameToVariableNode.contains(identifier))
  }

  /** Pushes a new method scope onto the scope stack.
    *
    * @param methodFullName
    *   The fully qualified name of the method.
    * @param methodName
    *   The name of the method.
    * @param scopeNode
    *   The node representing the method scope.
    * @param capturingRefId
    *   An optional node for capturing references.
    */
  def pushNewMethodScope(
    methodFullName: String,
    methodName: String,
    scopeNode: NewNode,
    capturingRefId: Option[NewNode]
  ): Unit = {
    stack = Option(MethodScopeElement(methodFullName, methodName, capturingRefId, scopeNode, stack))
  }

  /** Pushes a new block scope onto the scope stack.
    *
    * @param scopeNode
    *   The node representing the block scope.
    */
  def pushNewBlockScope(scopeNode: NewNode): Unit = {
    stack = Option(BlockScopeElement(scopeNode, stack))
  }

  /** Pops the current scope from the scope stack. */
  def popScope(): Unit = {
    stack = stack.flatMap(_.surroundingScope)
  }

  /** Adds a variable to the current scope.
    *
    * @param variableName
    *   The name of the variable.
    * @param variableNode
    *   The node representing the variable.
    * @param tpe
    *   The type of the variable.
    * @param scopeType
    *   The type of the scope (method or block).
    */
  def addVariable(variableName: String, variableNode: NewNode, tpe: String, scopeType: ScopeType): Unit = {
    addVariable(stack, variableName, variableNode, tpe, EvaluationStrategies.BY_REFERENCE, scopeType)
  }

  /** Adds a reference to a variable in the current scope.
    *
    * @param variableName
    *   The name of the variable.
    * @param referenceNode
    *   The node representing the reference.
    * @param tpe
    *   The type of the variable.
    * @param evaluationStrategy
    *   The evaluation strategy for the reference.
    */
  def addVariableReference(
    variableName: String,
    referenceNode: NewNode,
    tpe: String,
    evaluationStrategy: String
  ): Unit = {
    pendingReferences.prepend(PendingReference(variableName, referenceNode, tpe, evaluationStrategy, stack))
  }

  /** Updates the evaluation strategy of a variable reference.
    *
    * @param referenceNode
    *   The node representing the reference.
    * @param evaluationStrategy
    *   The new evaluation strategy.
    */
  def updateVariableReference(referenceNode: NewNode, evaluationStrategy: String): Unit = {
    pendingReferences.find(_.referenceNode == referenceNode).foreach(r => r.evaluationStrategy = evaluationStrategy)
  }

  /** The function establishes connections between variable references and their declarations in the CPG.
    *
    * It processes all pending variable references that were collected during AST traversal and attempts to match these
    * references with their variable declarations in the scope hierarchy. For unresolved references, it creates new
    * local variable nodes using the function `createLocalForUnresolvedReference`.
    *
    * The overall purpose of this function is to connect variable references to their declarations via REF edges in the
    * CPG It handles captured variables in closures by creating closure binding nodes. The function establishes the
    * correct variable scope relationships across method boundaries and ensures that all variable references are
    * properly linked in the graph, even when they refer to variables in outer scopes. This is essential for accurate
    * dataflow analysis, as it creates the edges that represent how data moves between variable definitions and their
    * uses throughout the program.
    *
    * @param diffGraph
    *   The diff graph builder.
    * @param filename
    *   The name of the file being processed.
    */
  def createVariableReferenceLinks(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val resolvedReferences = resolve(diffGraph, createLocalForUnresolvedReference)
    val capturedLocals     = mutable.HashMap.empty[String, NewNode]

    resolvedReferences.foreach { case VariableScopeManager.ResolvedReference(variableNodeId, origin) =>
      var nextReference: NewNode                  = null
      var maybeScopeElement: Option[ScopeElement] = origin.stack
      var currentReference: NewNode               = origin.referenceNode
      var done: Boolean                           = false
      while (!done) {
        val localOrCapturedLocalNodeOption =
          if (maybeScopeElement.exists(_.nameToVariableNode.contains(origin.variableName))) {
            done = true
            Option(variableNodeId)
          } else {
            maybeScopeElement.flatMap {
              case methodScope: VariableScopeManager.MethodScopeElement if methodScope.needsEnclosingScope =>
                maybeScopeElement = getEnclosingMethodScopeElement(maybeScopeElement)
                None
              case methodScope: VariableScopeManager.MethodScopeElement =>
                val prefix = if (methodScope.methodFullName.startsWith(filename)) { "" }
                else { s"$filename:" }
                val id = s"$prefix${methodScope.methodFullName}:${origin.variableName}"
                capturedLocals.updateWith(id) {
                  case None =>
                    val closureBinding = closureBindingNode(id, origin.evaluationStrategy)
                    methodScope.capturingRefNode.foreach(diffGraph.addEdge(_, closureBinding, EdgeTypes.CAPTURE))
                    nextReference = closureBinding
                    val localNode = createLocalForUnresolvedReference(diffGraph, methodScope.scopeNode, origin)
                    Option(localNode.closureBindingId(id))
                  case someLocalNode =>
                    done = true
                    someLocalNode
                }
              case _ => None
            }
          }

        localOrCapturedLocalNodeOption.foreach { localOrCapturedLocalNode =>
          transferLineAndColumnInfo(currentReference, localOrCapturedLocalNode)
          diffGraph.addEdge(currentReference, localOrCapturedLocalNode, EdgeTypes.REF)
          currentReference = nextReference
        }
        maybeScopeElement = maybeScopeElement.flatMap(_.surroundingScope)
      }
    }
  }

  /** Retrieves the enclosing method scope element from the given scope head.
    *
    * @param scopeHead
    *   The starting scope element.
    * @return
    *   The enclosing method scope element, if any.
    */
  protected def getEnclosingMethodScopeElement(scopeHead: Option[ScopeElement]): Option[MethodScopeElement] = {
    scopeHead.flatMap {
      case methodScope: MethodScopeElement => Some(methodScope)
      case blockScope: BlockScopeElement   => getEnclosingMethodScopeElement(blockScope.surroundingScope)
    }
  }

  /** Resolves pending references to variables by attempting to find their corresponding nodes in the scope stack.
    *
    * @param diffGraph
    *   The diff graph builder.
    * @param unresolvedHandler
    *   A handler function for unresolved references.
    * @return
    *   An iterator of resolved references.
    */
  private def resolve(
    diffGraph: DiffGraphBuilder,
    unresolvedHandler: (DiffGraphBuilder, NewNode, PendingReference) => NewLocal
  ): Iterator[ResolvedReference] = {
    pendingReferences.iterator.flatMap { pendingReference =>
      tryResolve(pendingReference).orElse {
        getEnclosingMethodScopeElement(pendingReference.stack).flatMap { methodScopeElement =>
          val newVariableNode = unresolvedHandler(diffGraph, methodScopeElement.scopeNode, pendingReference)
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

  /** Retrieves all enclosing method scope elements from the given scope head.
    *
    * @param scopeHead
    *   The starting scope element.
    * @return
    *   A sequence of enclosing method scope elements.
    */
  private def getAllEnclosingMethodScopeElements(scopeHead: Option[ScopeElement]): Seq[MethodScopeElement] = {
    scopeHead
      .collect {
        case methodScope: MethodScopeElement =>
          methodScope +: getAllEnclosingMethodScopeElements(methodScope.surroundingScope)
        case blockScope: BlockScopeElement =>
          getAllEnclosingMethodScopeElements(blockScope.surroundingScope)
      }
      .getOrElse(Seq.empty)
  }

  /** Transfers line and column information from one node to another.
    *
    * This method specifically handles transferring position data from identifier nodes to local variable nodes. It only
    * updates the target's position if it doesn't have position information yet or if the source has a later line
    * number.
    *
    * @param srcNode
    *   The source node containing the line and column information.
    * @param targetNode
    *   The target node to which the position information should be transferred.
    */
  private def transferLineAndColumnInfo(srcNode: NewNode, targetNode: NewNode): Unit = {
    (srcNode, targetNode) match {
      case (id: NewIdentifier, local: NewLocal) =>
        id.lineNumber match {
          case Some(srcLineNo) if local.lineNumber.isEmpty || !local.lineNumber.exists(_ < srcLineNo) =>
            // If there are multiple occurrences and the local is already set, ignore later updates
            local.lineNumber(id.lineNumber)
            local.columnNumber(id.columnNumber)
          case _ => // do nothing
        }
      case _ => // do nothing
    }

  }

  /** Creates a local variable node for an unresolved reference.
    *
    * @param diffGraph
    *   The diff graph builder.
    * @param methodScopeNodeId
    *   The node representing the method scope.
    * @param pendingReference
    *   The pending reference to resolve.
    * @return
    *   A new local variable node.
    */
  private def createLocalForUnresolvedReference(
    diffGraph: DiffGraphBuilder,
    methodScopeNodeId: NewNode,
    pendingReference: PendingReference
  ): NewLocal = {
    val name = pendingReference.variableName
    val tpe  = pendingReference.tpe
    val code = pendingReference.referenceNode match {
      case id: NewIdentifier => id.code
      case _                 => pendingReference.variableName
    }
    val local = localNodeWithExplicitPositionInfo(name, code, tpe).order(0)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    local
  }

  /** Retrieves a variable from the scope stack by its name.
    *
    * @param stack
    *   The current scope stack.
    * @param variableName
    *   The name of the variable.
    * @return
    *   An optional tuple containing the variable node, its type, and evaluation strategy.
    */
  private def variableFromStack(
    stack: Option[ScopeElement],
    variableName: String
  ): Option[(NewNode, String, String)] = {
    stack.flatMap { stackElement =>
      if (stackElement.nameToVariableNode.contains(variableName)) { stackElement.nameToVariableNode.get(variableName) }
      else { variableFromStack(stackElement.surroundingScope, variableName) }
    }
  }

  /** Adds a variable to the appropriate scope in the stack.
    *
    * @param stack
    *   The current scope stack.
    * @param variableName
    *   The name of the variable.
    * @param variableNode
    *   The node representing the variable.
    * @param tpe
    *   The type of the variable.
    * @param evaluationStrategy
    *   The evaluation strategy for the variable.
    * @param scopeType
    *   The type of the scope (method or block).
    */
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

  /** Attempts to resolve a pending reference to a variable.
    *
    * @param pendingReference
    *   The pending reference to resolve.
    * @return
    *   An optional resolved reference.
    */
  private def tryResolve(pendingReference: PendingReference): Option[ResolvedReference] = {
    variableFromStack(pendingReference.stack, pendingReference.variableName).map { case (variableNodeId, _, _) =>
      ResolvedReference(variableNodeId, pendingReference)
    }
  }

}
