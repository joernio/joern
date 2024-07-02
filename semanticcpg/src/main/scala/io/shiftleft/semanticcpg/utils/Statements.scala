package io.shiftleft.semanticcpg.utils

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

object Statements {
  def countAll(cpg: Cpg): Long =
    cpg.method.topLevelExpressions.size
}
