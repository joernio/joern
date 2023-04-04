package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.MapView
import scala.collection.concurrent.TrieMap

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

  private val table = TrieMap.empty[K, Set[String]]

  def apply(sbKey: K): Set[String] = table(sbKey)

  def apply(node: AstNode): Set[String] =
    keyFromNode(node) match {
      case Some(key) => table(key)
      case None      => Set.empty
    }

  def from(sb: IterableOnce[(K, Set[String])]): SymbolTable[K] = {
    table.addAll(sb); this
  }

  def put(sbKey: K, typeFullNames: Set[String]): Set[String] = {
    if (typeFullNames.nonEmpty)
      table.put(sbKey, typeFullNames).getOrElse(Set.empty)
    else
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

  def append(node: AstNode, typeFullNames: Set[String]): Set[String] = keyFromNode(node) match {
    case Some(key) => append(key, typeFullNames)
    case None      => Set.empty
  }

  def append(sbKey: K, typeFullNames: Set[String]): Set[String] = {
    table.get(sbKey) match {
      case Some(ts) if ts == typeFullNames    => ts
      case Some(ts) if typeFullNames.nonEmpty => put(sbKey, ts ++ typeFullNames)
      case None if typeFullNames.nonEmpty     => put(sbKey, typeFullNames)
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

  def view: MapView[K, Set[String]] = table.view

  def clear(): Unit = table.clear()

}
