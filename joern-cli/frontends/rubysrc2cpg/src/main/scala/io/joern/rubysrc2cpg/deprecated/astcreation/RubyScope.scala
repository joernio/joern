package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.x2cpg.datastructures.Scope
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{DeclarationNew, NewIdentifier, NewLocal, NewNode}

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

  override def popScope(): Option[NewNode] = {
    stack.headOption.map(_.scopeNode).foreach(scopeToVarMap.remove)
    super.popScope()
  }

  /** Will generate local nodes for this scope's variables, excluding those that reference parameters.
    * @param paramNames
    *   the names of parameters.
    */
  def createAndLinkLocalNodes(diffGraph: DiffGraphBuilder, paramNames: Set[String] = Set.empty): List[DeclarationNew] =
    stack.headOption match
      case Some(top) => scopeToVarMap.buildVariableGroupings(top.scopeNode, paramNames ++ Set("this"), diffGraph)
      case None      => List.empty[DeclarationNew]

      /** @param identifier
        *   the identifier to count
        * @return
        *   the number of times the given identifier occurs in the immediate scope.
        */
  def numVariableReferences(identifier: String): Int = {
    stack.map(_.scopeNode).flatMap(scopeToVarMap.get).flatMap(_.get(identifier)).map(_.ids.size).headOption.getOrElse(0)
  }

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

    /** Will persist the variable groupings that do not represent parameter nodes and link them with REF edges.
      * @return
      *   the list of persisted local nodes.
      */
    def buildVariableGroupings(
      key: ScopeNodeType,
      paramNames: Set[String],
      diffGraph: DiffGraphBuilder
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
  }

}
