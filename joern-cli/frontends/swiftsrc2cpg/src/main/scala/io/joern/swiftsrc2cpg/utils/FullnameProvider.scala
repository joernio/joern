package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.joern.swiftsrc2cpg.utils.FullnameProvider.NodeKindMapping
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.{ResolvedTypeInfo, SwiftFileLocalTypeMapping}

import scala.annotation.tailrec

/** Companion object for the FullnameProvider class. Contains helper types and constants used by the FullnameProvider.
  */
private object FullnameProvider {

  /** Represents the kind of fullName to retrieve.
    *   - Type - For retrieving type fullNames
    *   - Decl - For retrieving declaration fullNames
    */
  private enum Kind {
    case Type, Decl
  }

  // TODO: provide the actual mapping from SwiftNode.toString (nodeKind) to ResolvedTypeInfo.nodeKind priority list
  private val NodeKindMapping = Map(
    "DeclReferenceExprSyntax" -> List("type_expr", "member_ref_expr"),
    "VariableDeclSyntax"      -> List("var_decl"),
    "PatternBindingSyntax"    -> List("var_decl"),
    "IdentifierPatternSyntax" -> List("var_decl")
  )
}

/** Provides functionality to resolve and retrieve fullNames for Swift types and declarations. Uses a type mapping to
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
  private def filterForNodeKind(in: Set[ResolvedTypeInfo], nodeKind: String): Option[ResolvedTypeInfo] = {
    if (in.isEmpty) return None
    if (in.size == 1) return in.headOption
    NodeKindMapping
      .get(nodeKind)
      .flatMap(mappedNodeKinds => mappedNodeKinds.flatMap(kind => in.find(_.nodeKind == kind)).headOption)
      .orElse(in.headOption)
  }

  /** Recursively attempts to find the fullName for a given source range and kind. If the exact range is not found, it
    * narrows the range down to a single point (the Swift compiler may generate synthetic node for them).
    *
    * @param range
    *   The source position range (start offset, end offset)
    * @param kind
    *   The kind of fullName to retrieve (Type or Decl)
    * @param nodeKind
    *   The kind of Swift node
    * @return
    *   An optional String containing the fullName if found
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

  /** Retrieves the type fullName for a given source range and node kind.
    *
    * @param range
    *   The source position range (start offset, end offset)
    * @param nodeKind
    *   The kind of Swift node
    * @return
    *   An optional String containing the type fullName if found
    */
  protected def typeFullname(range: (Int, Int), nodeKind: String): Option[String] = {
    fullName(range, FullnameProvider.Kind.Type, nodeKind).map(AstCreatorHelper.cleanType)
  }

  /** Same as FullnameProvider.typeFullname but does no type name sanitation.
    */
  protected def typeFullnameRaw(range: (Int, Int), nodeKind: String): Option[String] = {
    fullName(range, FullnameProvider.Kind.Type, nodeKind).map(AstCreatorHelper.cleanName)
  }

  /** Retrieves the declaration fullName for a given source range and node kind.
    *
    * @param range
    *   The source position range (start offset, end offset)
    * @param nodeKind
    *   The kind of Swift node
    * @return
    *   An optional String containing the declaration fullName if found
    */
  protected def declFullname(range: (Int, Int), nodeKind: String): Option[String] = {
    fullName(range, FullnameProvider.Kind.Decl, nodeKind)
  }

  /** Retrieves the type fullName for a given Swift node. Extracts the start and end offsets from the node if available.
    * Returns None if typeMap is empty.
    *
    * @param node
    *   The Swift node to get the type fullName for
    * @return
    *   An optional String containing the type fullName if found
    */
  def typeFullname(node: SwiftNode): Option[String] = {
    if (typeMap.isEmpty) return None
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) => typeFullname((start, end), node.toString)
      case _                        => None
    }
  }

  /** Same as FullnameProvider.typeFullname but does no type name sanitation.
    */
  def typeFullnameRaw(node: SwiftNode): Option[String] = {
    if (typeMap.isEmpty) return None
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) => typeFullnameRaw((start, end), node.toString)
      case _                        => None
    }
  }

  /** Retrieves the declaration fullName for a given Swift node. Extracts the start and end offsets from the node if
    * available. Returns None if typeMap is empty.
    *
    * @param node
    *   The Swift node to get the declaration fullName for
    * @return
    *   An optional String containing the declaration fullName if found
    */
  def declFullname(node: SwiftNode): Option[String] = {
    declFullnameRaw(node).map(_.replace("<extension>", ""))
  }

  /** Same as FullnameProvider.declFullname but does not strip the `<extension>` tag.
    */
  def declFullnameRaw(node: SwiftNode): Option[String] = {
    if (typeMap.isEmpty) return None
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) => declFullname((start, end), node.toString)
      case _                        => None
    }
  }

  /** Returns inheritance fullNames for the given Swift node.
    *
    * Looks up resolved type information in the `typeMap` using the node's start and end offsets. If no mapping is
    * present or the node does not have offsets, an empty sequence is returned.
    *
    * @param node
    *   the Swift AST node to query
    * @return
    *   a sequence of inheritance fullNames, or an empty sequence if none are found
    */
  def inheritsFor(node: SwiftNode): Seq[String] = {
    if (typeMap.isEmpty) return Seq.empty
    (node.startOffset, node.endOffset) match {
      case (Some(start), Some(end)) =>
        typeMap.get((start, end)) match {
          case Some(typeInfos) =>
            typeInfos.flatMap(_.inherits).toSeq
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

}
