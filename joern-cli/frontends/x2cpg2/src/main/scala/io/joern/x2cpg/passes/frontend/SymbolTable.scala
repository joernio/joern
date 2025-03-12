package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.MapView
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/** Represents an identifier of some AST node at a specific scope.
  */
abstract class SBKey(val identifier: String) {

  /** Convenience methods to convert a node to a [[SBKey]].
    *
    * @param node
    *   the node to convert.
    * @return
    *   the corresponding [[SBKey]] if the node is supported as a key variable
    */
  def fromNode(node: AstNode): Option[SBKey]

}

object SBKey {
  protected val logger: Logger = LoggerFactory.getLogger(getClass)
  def fromNodeToLocalKey(node: AstNode): Option[LocalKey] = {
    Option(node match {
      case n: Identifier => LocalVar(n.name)
      case n: Local      => LocalVar(n.name)
      case n: Call =>
        CallAlias(n.name, n.argument.collectFirst { case x: Identifier if x.argumentIndex == 0 => x.name })
      case n: Method            => CallAlias(n.name, Option("this"))
      case n: MethodRef         => CallAlias(n.code)
      case n: FieldIdentifier   => LocalVar(n.canonicalName)
      case n: MethodParameterIn => LocalVar(n.name)
      case _ => logger.debug(s"Local node of type ${node.label} is not supported in the type recovery pass."); null
    })
  }

}

/** Represents an identifier of some AST node at an intraprocedural scope.
  */
sealed class LocalKey(identifier: String) extends SBKey(identifier) {
  override def fromNode(node: AstNode): Option[SBKey] = SBKey.fromNodeToLocalKey(node)
}

/** A variable that holds data within an intraprocedural scope.
  */
case class LocalVar(override val identifier: String) extends LocalKey(identifier)

/** A collection object that can be accessed with potentially dynamic keys and values.
  */
case class CollectionVar(override val identifier: String, idx: String) extends LocalKey(identifier)

/** A name that refers to some kind of callee.
  */
case class CallAlias(override val identifier: String, receiverName: Option[String] = None) extends LocalKey(identifier)

/** A thread-safe symbol table that can represent multiple types per symbol. Each node in an AST gets converted to an
  * [[SBKey]] which gives contextual information to identify an AST entity. Each value in this table represents a set of
  * types that the key could be in a flow-insensitive manner.
  *
  * The [[SymbolTable]] operates like a map with a few convenient methods that are designed for this structure's
  * purpose.
  */
class SymbolTable[K <: SBKey](val keyFromNode: AstNode => Option[K]) {

  private val table = mutable.HashMap.empty[K, Set[String]]

  /** The set limit is to bound the set of possible types, since by using dummy types we could have an unbounded number
    * of permutations of various access paths
    */
  private val setLimit = 10

  private def coalesce(oldEntries: Set[String], newEntries: Set[String]): Set[String] = {
    val allTypes = (oldEntries ++ newEntries).toSeq // convert to ordered set to make `take` work predictably
    val (dummies, noDummies) = allTypes.partition(XTypeRecovery.isDummyType)
    (noDummies ++ dummies).take(setLimit).toSet
  }

  def apply(sbKey: K): Set[String] = table(sbKey)

  def apply(node: AstNode): Set[String] =
    keyFromNode(node) match {
      case Some(key) => table(key)
      case None      => Set.empty
    }

  def from(sb: IterableOnce[(K, Set[String])]): SymbolTable[K] = {
    table.addAll(sb); this
  }

  def put(sbKey: K, typeFullNames: Set[String]): Set[String] =
    if (typeFullNames.nonEmpty) {
      val newEntry = coalesce(Set.empty, typeFullNames)
      table.put(sbKey, newEntry)
      newEntry
    } else {
      Set.empty
    }

  def put(sbKey: K, typeFullName: String): Set[String] =
    put(sbKey, Set(typeFullName))

  def put(node: AstNode, typeFullNames: Set[String]): Set[String] = keyFromNode(node) match {
    case Some(key) => put(key, typeFullNames)
    case None      => Set.empty
  }

  def append(node: AstNode, typeFullName: String): Set[String] =
    append(node, Set(typeFullName))

  def append(node: K, typeFullName: String): Set[String] =
    append(node, Set(typeFullName))

  def append(node: AstNode, typeFullNames: Set[String]): Set[String] = keyFromNode(node) match {
    case Some(key) => append(key, typeFullNames)
    case None      => Set.empty
  }

  def append(sbKey: K, typeFullNames: Set[String]): Set[String] = {
    table.get(sbKey) match {
      case Some(ts) if ts == typeFullNames    => ts
      case Some(ts) if typeFullNames.nonEmpty => put(sbKey, coalesce(ts, typeFullNames))
      case None if typeFullNames.nonEmpty     => put(sbKey, coalesce(Set.empty, typeFullNames))
      case _                                  => Set.empty
    }
  }

  def contains(sbKey: K): Boolean = table.contains(sbKey)

  def contains(node: AstNode): Boolean = keyFromNode(node) match {
    case Some(key) => contains(key)
    case None      => false
  }

  def get(sbKey: K): Set[String] = table.getOrElse(sbKey, Set.empty)

  def get(node: AstNode): Set[String] = keyFromNode(node) match {
    case Some(key) => get(key)
    case None      => Set.empty
  }

  def remove(sbKey: K): Set[String] = table.remove(sbKey).getOrElse(Set.empty)

  def remove(node: AstNode): Set[String] = keyFromNode(node) match {
    case Some(key) => remove(key)
    case None      => Set.empty
  }

  def itemsCopy: mutable.ArrayBuffer[(K, Set[String])] = mutable.ArrayBuffer.from(table.iterator)
}
