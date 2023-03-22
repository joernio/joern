package io.joern

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Identifier}
import io.shiftleft.semanticcpg.language._

package object dataflowengineoss {

  def identifierToFirstUsages(node : Identifier): List[Identifier] = node
    .refsTo.flatMap(identifiersFromCapturedScopes).l

  def identifiersFromCapturedScopes(i: Declaration): List[Identifier] =
    i.capturedByMethodRef.referencedMethod.ast.isIdentifier
      .nameExact(i.name)
      .sortBy(x => (x.lineNumber, x.columnNumber))
      .l

}
