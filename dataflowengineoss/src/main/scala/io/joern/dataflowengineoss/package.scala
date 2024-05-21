package io.joern

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Expression, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

package object dataflowengineoss {

  def globalFromLiteral(lit: Literal): Iterator[Expression] = lit.start.inCall.assignment
    .where(_.method.isModule)
    .argument(1)

  def identifierToFirstUsages(node: Identifier): List[Identifier] = node.refsTo.flatMap(identifiersFromCapturedScopes).l

  def identifiersFromCapturedScopes(i: Declaration): List[Identifier] =
    i.capturedByMethodRef.referencedMethod.ast.isIdentifier
      .nameExact(i.name)
      .sortBy(x => (x.lineNumber, x.columnNumber))
      .l

}
