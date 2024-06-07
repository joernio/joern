package io.joern

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Expression, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import io.shiftleft.semanticcpg.utils.MemberAccess.isFieldAccess

package object dataflowengineoss {

  /** Returns the target of an assignment involving [[lit]], if the assignment is found inside a module method.
    *
    * @param lit
    *   the literal to appear on the RHS of the assignment
    * @param recursive
    *   should [[lit]] be the sole RHS (false) or can it be an arbitrarily nested sub-expression (true)
    * @return
    *   the LHS of the assignment
    */
  def globalFromLiteral(lit: Literal, recursive: Boolean = true): Iterator[Expression] = {

    /** Frontends often create three-address code representations of compound literals, e.g. in pysrc2cpg the dictionary
      * literal `{"x": y}` is lowered as a block `{tmp0 = {}; tmp0["x"] = y; tmp0}`.
      *
      * In an assignment `foo = {"x": "y"}`, if [[lit]] is "y", we don't want to pick the intermediate assignment
      * `tmp0["x"] = "y"`, since `tmp0` is never a global/module-level variable. Instead, we want to pick `foo`.
      */
    def skipLowLevelAssignments(assignments: Iterator[Assignment]): Iterator[Assignment] = {
      assignments.repeat(_.parentBlock.inCall.isAssignment)(_.emit).lastOption.fold(assignments)(_.iterator)
    }

    lit.start
      .where(_.method.isModule)
      .flatMap(t => if (recursive) t.inAssignment else skipLowLevelAssignments(t.inCall.isAssignment))
      .target
  }

  def identifierToFirstUsages(node: Identifier): List[Identifier] = node.refsTo.flatMap(identifiersFromCapturedScopes).l

  def identifiersFromCapturedScopes(i: Declaration): List[Identifier] =
    i.capturedByMethodRef.referencedMethod.ast.isIdentifier
      .nameExact(i.name)
      .sortBy(x => (x.lineNumber, x.columnNumber))
      .l

}
