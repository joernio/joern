package io.joern.rubysrc2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Scope
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import overflowdb.BatchedUpdate

import scala.collection.mutable

/** Extends the Scope class to help scope variables and create locals.
  *
  * TODO: Extend this to similarly link parameter nodes (especially `this` node) for consistency.
  */
class RubyScope extends Scope[String, NewIdentifier, NewNode] {

  private type VarMap        = Map[String, VarGroup]
  private type ScopeNodeType = NewNode

  /** Groups a local node with its referencing identifiers.
    */
  private case class VarGroup(local: NewLocal, ids: List[NewIdentifier])

  /** Links a scope to its variable groupings.
    */
  private val scopeToVarMap = mutable.HashMap.empty[ScopeNodeType, VarMap]

  override def addToScope(identifier: String, variable: NewIdentifier): NewNode = {
    val scopeNode = super.addToScope(identifier, variable)
    stack.headOption.foreach(head => scopeToVarMap.appendIdentifierToVarGroup(head.scopeNode, variable))
    scopeNode
  }

  def removeFromScope(variable: NewIdentifier): Unit = {
    stack.headOption.foreach(head => scopeToVarMap.removeIdentifierFromVarGroup(head.scopeNode, variable))
  }

  override def popScope(): Option[NewNode] = {
    stack.headOption.map(_.scopeNode).foreach(scopeToVarMap.remove)
    super.popScope()
  }

  /** Will generate local nodes for this scope's variables, excluding those that reference parameters.
    * @param paramNames
    *   the names of parameters.
    */
  def createAndLinkLocalNodes(
    diffGraph: BatchedUpdate.DiffGraphBuilder,
    paramNames: Set[String] = Set.empty
  ): List[DeclarationNew] = {
    stack.headOption match
      case Some(top) => scopeToVarMap.buildVariableGroupings(top.scopeNode, paramNames ++ Set("this"), diffGraph)
      case None      => List.empty[DeclarationNew]
  }

  /** Links the parameter node to the referenced identifiers in this scope.
    */
  def linkParamNode(diffGraph: BatchedUpdate.DiffGraphBuilder, param: NewMethodParameterIn): Unit =
    stack.headOption match
      case Some(top) => scopeToVarMap.buildParameterGrouping(top.scopeNode, param, diffGraph)
      case None      => List.empty[DeclarationNew]

  private implicit class IdentifierExt(node: NewIdentifier) {

    /** Creates a new VarGroup and corresponding NewLocal for the given identifier.
      */
    def toNewVarGroup: VarGroup = {
      val newLocal = NewLocal()
        .name(node.name)
        .code(node.name)
        .lineNumber(node.lineNumber)
        .columnNumber(node.columnNumber)
        .typeFullName(node.typeFullName)
      VarGroup(newLocal, List(node))
    }

  }

  private implicit class ScopeExt(scopeMap: mutable.Map[ScopeNodeType, VarMap]) {

    /** Registers the identifier to its corresponding variable grouping in the given scope.
      */
    def appendIdentifierToVarGroup(key: ScopeNodeType, identifier: NewIdentifier): Unit =
      scopeMap.updateWith(key) {
        case Some(varMap: VarMap) =>
          Some(varMap.updatedWith(identifier.name) {
            case Some(varGroup: VarGroup) => Some(varGroup.copy(ids = varGroup.ids :+ identifier))
            case None                     => Some(identifier.toNewVarGroup)
          })
        case None =>
          Some(Map(identifier.name -> identifier.toNewVarGroup))
      }

    /** Removes an identifier from the var group.
      */
    def removeIdentifierFromVarGroup(key: ScopeNodeType, identifier: NewIdentifier): Unit =
      scopeMap.updateWith(key) {
        case Some(varMap: VarMap) =>
          Some(varMap.updatedWith(identifier.name) {
            case Some(varGroup: VarGroup) => Some(varGroup.copy(ids = varGroup.ids.filterNot(_ == identifier)))
            case None                     => None
          })
        case None => None
      }

    /** Will persist the variable groupings that do not represent parameter nodes and link them with REF edges.
      * @return
      *   the list of persisted local nodes.
      */
    def buildVariableGroupings(
      key: ScopeNodeType,
      paramNames: Set[String],
      diffGraph: BatchedUpdate.DiffGraphBuilder
    ): List[DeclarationNew] =
      scopeMap.get(key) match
        case Some(varMap) =>
          varMap.values
            .filterNot { case VarGroup(local, _) => paramNames.contains(local.name) }
            .map { case VarGroup(local, ids) =>
              ids.foreach(id => diffGraph.addEdge(id, local, EdgeTypes.REF))
              local
            }
            .toList
        case None => List.empty[DeclarationNew]

    /** Will persist a REF edge between the given parameter and its corresponding identifiers.
      */
    def buildParameterGrouping(
      key: ScopeNodeType,
      param: NewMethodParameterIn,
      diffGraph: BatchedUpdate.DiffGraphBuilder
    ): Unit = {
      scopeMap
        .get(key)
        .map(_.values)
        .foreach(_.filter { case VarGroup(local, _) => local.name == param.name }
          .foreach { case VarGroup(_, ids) =>
            ids.foreach(id => diffGraph.addEdge(id, param, EdgeTypes.REF))
          })
    }

  }

}
