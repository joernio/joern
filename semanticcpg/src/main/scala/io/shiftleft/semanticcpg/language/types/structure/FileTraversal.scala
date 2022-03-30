package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import overflowdb.traversal._

/** A compilation unit
  */
class FileTraversal(val traversal: Traversal[File]) extends AnyVal {

  def namespace: Traversal[Namespace] =
    traversal.in(EdgeTypes.SOURCE_FILE).hasLabel(NodeTypes.NAMESPACE_BLOCK).out(EdgeTypes.REF).cast[Namespace]

}

object FileTraversal {

  val UNKNOWN = "<unknown>"

}
