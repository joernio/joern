package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

class TargetTraversal(val traversal: Iterator[Expression]) extends AnyVal {

  @Doc(
    info = "Outer-most array access",
    longInfo = """
        Array access at highest level , e.g., in a(b(c)), the entire expression
        is returned, but not b(c) alone.
        """
  )
  def arrayAccess: Iterator[OpNodes.ArrayAccess] = traversal.flatMap(_.arrayAccess)

  @Doc(info = "Returns 'pointer' in assignments of the form *(pointer) = x")
  def pointer: Iterator[Expression] = traversal.flatMap(_.pointer)

}
