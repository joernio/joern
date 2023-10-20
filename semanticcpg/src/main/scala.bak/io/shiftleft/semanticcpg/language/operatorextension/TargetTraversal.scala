package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language.*
// TODO bring back: import overflowdb.traversal.help.Doc

class TargetTraversal(val traversal: Iterator[Expression]) extends AnyVal {

  // TODO bring back: @Doc(
    info = "Outer-most array access",
    longInfo = """
        Array access at highest level , e.g., in a(b(c)), the entire expression
        is returned, but not b(c) alone.
        """
  )
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.arrayAccess)

  // TODO bring back: @Doc(info = "Returns 'pointer' in assignments of the form *(pointer) = x")
  def pointer: Iterator[Expression] = traversal.flatMap(_.pointer)

}
