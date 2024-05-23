package io.joern

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Expression, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

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
  def globalFromLiteral(lit: Literal, recursive: Boolean = true): Iterator[Expression] = lit.start
    .where(_.method.isModule)
    .flatMap(t => if (recursive) t.inAssignment else t.inCall.assignment)
    .target

  def identifierToFirstUsages(node: Identifier): List[Identifier] = node.refsTo.flatMap(identifiersFromCapturedScopes).l

  def identifiersFromCapturedScopes(i: Declaration): List[Identifier] =
    i.capturedByMethodRef.referencedMethod.ast.isIdentifier
      .nameExact(i.name)
      .sortBy(x => (x.lineNumber, x.columnNumber))
      .l

}
