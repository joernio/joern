package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  FieldIdentifier,
  Identifier,
  Local,
  Method,
  MethodRef
}
import org.slf4j.{Logger, LoggerFactory}
import io.shiftleft.semanticcpg.language._

import java.util.Objects
import scala.collection.MapView
import scala.collection.concurrent.TrieMap
import scala.util.hashing.Hashing

/** Represents an identifier of some AST node at a specific scope.
  */
abstract class SBKey {

  /** Convenience methods to convert a node to a [[SBKey]].
    *
    * @param node
    *   the node to convert.
    * @return
    *   the corresponding [[SBKey]].
    */
  def fromNode(node: AstNode): SBKey

}

object SBKey {
  protected val logger: Logger = LoggerFactory.getLogger(getClass)
  def fromNodeToLocalKey(node: AstNode): LocalKey = {
    node match {
      case n: FieldIdentifier => FieldVar(n.canonicalName)
      case n: Identifier      => LocalVar(n.name)
      case n: Local           => LocalVar(n.name)
      case n: Call            => CallAlias(n.name)
      case n: Method          => CallAlias(n.name)
      case n: MethodRef       => CallAlias(n.code)
      case _ =>
        throw new RuntimeException(s"Local node of type ${node.label} is not supported in the type recovery pass.")
    }
  }

  def fromNodeToGlobalKey(node: AstNode): GlobalKey = node match {
    case n: FieldIdentifier => Field(n.method.fullName, n.canonicalName)
    case n: Identifier      => Field(n.method.fullName, n.name)
    case _ =>
      throw new RuntimeException(s"Global node of type ${node.label} is not supported in the type recovery pass.")
  }

}

/** Represents an identifier of some AST node at an intraprocedural scope.
  */
sealed class LocalKey extends SBKey {
  override def fromNode(node: AstNode): SBKey = SBKey.fromNodeToLocalKey(node)
}

/** A variable that can hold data within an interprocedural scope.
  */
case class FieldVar(identifier: String) extends LocalKey

/** A variable that holds data within an intraprocedural scope.
  */
case class LocalVar(identifier: String) extends LocalKey

/** A name that refers to some kind of callee.
  */
case class CallAlias(identifier: String) extends LocalKey

/** Represents an identifier of some AST node at an interprocedural scope.
  */
sealed class GlobalKey extends SBKey {
  override def fromNode(node: AstNode): SBKey = SBKey.fromNodeToGlobalKey(node)
}

case class Field(compUnitFullName: String, identifier: String) extends GlobalKey {
  def fullName = s"$compUnitFullName.$identifier"

  override def equals(obj: Any): Boolean = obj match {
    case o: Field => o.identifier.equals(identifier) && o.compUnitFullName.equals(compUnitFullName)
    case _        => false
  }

  override def hashCode(): Int = Objects.hash(compUnitFullName, identifier)
}

/** A thread-safe symbol table that can represent multiple types per symbol. Each node in an AST gets converted to an
  * [[SBKey]] which gives contextual information to identify an AST entity. Each value in this table represents a set of
  * types that the key could be in a flow-insensitive manner.
  *
  * The [[SymbolTable]] operates like a map with a few convenient methods that are designed for this structure's
  * purpose.
  */
class SymbolTable[K <: SBKey](fromNode: AstNode => K) {

  private val table = TrieMap.empty[K, Set[String]]

  def apply(sbKey: K): Set[String] = table(sbKey)

  def apply(node: AstNode): Set[String] = table(fromNode(node))

  def from(sb: IterableOnce[(K, Set[String])]): SymbolTable[K] = {
    table.addAll(sb); this
  }

  def put(sbKey: K, typeFullNames: Set[String]): Option[Set[String]] =
    table.put(sbKey, typeFullNames)

  def put(sbKey: K, typeFullName: String): Option[Set[String]] =
    put(sbKey, Set(typeFullName))

  def put(node: AstNode, typeFullNames: Set[String]): Option[Set[String]] =
    put(fromNode(node), typeFullNames)

  def append(node: AstNode, typeFullName: String): Option[Set[String]] =
    append(node, Set(typeFullName))

  def append(node: AstNode, typeFullNames: Set[String]): Option[Set[String]] =
    append(fromNode(node), typeFullNames)

  private def append(sbKey: K, typeFullNames: Set[String]): Option[Set[String]] = {
    table.get(sbKey) match {
      case Some(ts) => table.put(sbKey, ts ++ typeFullNames)
      case None     => table.put(sbKey, typeFullNames)
    }
  }

  def contains(sbKey: K): Boolean = table.contains(sbKey)

  def contains(node: AstNode): Boolean = contains(fromNode(node))

  def get(sbKey: K): Set[String] = table.getOrElse(sbKey, Set.empty)

  def get(node: AstNode): Set[String] = get(fromNode(node))

  def view: MapView[K, Set[String]] = table.view

  def clear(): Unit = table.clear()

}
