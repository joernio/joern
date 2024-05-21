package io.joern

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Expression, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

package object dataflowengineoss {

  def globalFromLiteral(lit: Literal): Iterator[Expression] = {
    val relevantLiteral = lit.start.where(_.method.isModule).l

    // Gets <x> from `<x> = <lit>`
    val lhsOfAssignment = relevantLiteral.inCall.assignment.argument(1)

    // Gets <x> from `<x> = import(...<lit>...)` because of how Python imports are represented
    val lhsOfImport = relevantLiteral.inCall.nameExact("import").inCall.assignment.argument(1)

    lhsOfAssignment ++ lhsOfImport
  }

  def identifierToFirstUsages(node: Identifier): List[Identifier] = node.refsTo.flatMap(identifiersFromCapturedScopes).l

  def identifiersFromCapturedScopes(i: Declaration): List[Identifier] =
    i.capturedByMethodRef.referencedMethod.ast.isIdentifier
      .nameExact(i.name)
      .sortBy(x => (x.lineNumber, x.columnNumber))
      .l

}
