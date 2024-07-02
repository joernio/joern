package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** A compilation unit
  */
class FileTraversal(val traversal: Iterator[File]) extends AnyVal {

  def namespace: Iterator[Namespace] =
    traversal.flatMap(_.namespaceBlock).flatMap(_._namespaceViaRefOut)

}

object FileTraversal {
  val UNKNOWN = "<unknown>"
}
