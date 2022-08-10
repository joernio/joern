package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.traversal._

/** A compilation unit
  */
class FileTraversal(val traversal: Traversal[File]) extends AnyVal {

  def namespace: Traversal[Namespace] =
    traversal.flatMap(_.namespaceBlock).flatMap(_._namespaceViaRefOut)

}

object FileTraversal {
  val UNKNOWN = "<unknown>"
}
