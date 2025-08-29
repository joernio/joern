package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.joern.swiftsrc2cpg.utils.FullnameProvider.NodeKindMapping
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.{ResolvedTypeInfo, SwiftFileLocalTypeMapping}

import scala.annotation.tailrec
import scala.collection.mutable

/** Companion object for the FullnameProvider class. Contains helper types and constants used by the FullnameProvider.
  */
private object FullnameProvider {

  /** Represents the kind of fullname to retrieve.
    *   - Type - For retrieving type fullnames
    *   - Decl - For retrieving declaration fullnames
    */
  private enum Kind {
    case Type, Decl
  }

  // TODO: provide the actual mapping from SwiftNode.toString (nodeKind) to ResolvedTypeInfo.nodeKind
  private val NodeKindMapping = Map("DeclReferenceExprSyntax" -> "type_expr")
}

/** Provides functionality to resolve and retrieve fullnames for Swift types and declarations. Uses a type mapping to
  * resolve Swift node references to their fully qualified names.
  *
  * @param typeMap
  *   A mapping from source position ranges to resolved type information
  */
class FullnameProvider(typeMap: SwiftFileLocalTypeMapping) {

  /** Filters a set of resolved type information based on the given node kind.
    *
    * @param in
    *   The set of resolved type information to filter
    * @param nodeKind
    *   The kind of Swift node to match
    * @return
    *   An optional resolved type information that matches the node kind
    */
  private def filterForNodeKind(in: mutable.HashSet[ResolvedTypeInfo], nodeKind: String): Option[ResolvedTypeInfo] = {
    if (in.isEmpty) return None
    if (in.size == 1) return in.headOption
    NodeKindMapping.get(nodeKind).flatMap(mappedNodeKind => in.find(_.nodeKind == mappedNodeKind)).orElse(in.headOption)
  }

  /** Recursively attempts to find the fullname for a given source range and kind. If the exact range is not found, it
    * narrows the range down to a single point (the Swift compiler may generate synthetic node for them).
    *
    * @param range
    *   The source position range (start offset, end offset)
    * @param kind
    *   The kind of fullname to retrieve (Type or Decl)
    * @param nodeKind
    *   The kind of Swift node
    * @return
    *   An optional String containing the fullname if found
    */
  @tailrec
  private def fullName(
    range: (Int, Int),
    kind: FullnameProvider.Kind,
    nodeKind: String,
    iter: Int = 1
  ): Option[String] = {
    typeMap.get(range) match {
      case Some(typeInfo) if kind == FullnameProvider.Kind.Type =>
        filterForNodeKind(typeInfo.filter(_.typeFullname.nonEmpty), nodeKind).flatMap(_.typeFullname)
      case Some(typeInfo) if kind == FullnameProvider.Kind.Decl =>
        filterForNodeKind(typeInfo.filter(_.declFullname.nonEmpty), nodeKind).flatMap(_.declFullname)
      case None if range._1 != range._2 && iter > 0 =>
        // Only recurse if we haven't already considered offsets (for synthetic AST elements with +-1 offsets)
        fullName((range._1 - 1, range._2 + 1), kind, nodeKind, 0)
      case None if range._1 != range._2 =>
        // Only recurse if we haven't already reduced to a point (for synthetic AST elements)
        fullName((range._1 + 1, range._1 + 1), kind, nodeKind)
      case _ =>
        // We've already tried with a point and still found nothing
        None
    }
  }

  /** Retrieves the type fullname for a given source range and node kind.
    *
    * @param range
    *   The source position range (start offset, end offset)
    * @param nodeKind
    *   The kind of Swift node
    * @return
    *   An optional String containing the type fullname if found
    */
  protected def typeFullname(range: (Int, Int), nodeKind: String): Option[String] = {
    fullName(range, FullnameProvider.Kind.Type, nodeKind).map(AstCreatorHelper.cleanName)
  }

  /** Retrieves the declaration fullname for a given source range and node kind.
    *
    * @param range
    *   The source position range (start offset, end offset)
    * @param nodeKind
    *   The kind of Swift node
    * @return
    *   An optional String containing the declaration fullname if found
    */
  protected def declFullname(range: (Int, Int), nodeKind: String): Option[String] = {
    fullName(range, FullnameProvider.Kind.Decl, nodeKind).map(AstCreatorHelper.cleanName)
  }

  /** Retrieves the type fullname for a given Swift node. Extracts the start and end offsets from the node if available.
    * Returns None if typeMap is empty.
    *
    * @param node
    *   The Swift node to get the type fullname for
    * @return
    *   An optional String containing the type fullname if found
    */
  def typeFullname(node: SwiftNode): Option[String] = {
    if (typeMap.isEmpty) return None
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) => typeFullname((start, end), node.toString)
      case _                        => None
    }
  }

  /** Retrieves the declaration fullname for a given Swift node. Extracts the start and end offsets from the node if
    * available. Returns None if typeMap is empty.
    *
    * @param node
    *   The Swift node to get the declaration fullname for
    * @return
    *   An optional String containing the declaration fullname if found
    */
  def declFullname(node: SwiftNode): Option[String] = {
    if (typeMap.isEmpty) return None
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) => declFullname((start, end), node.toString)
      case _                        => None
    }
  }

}
