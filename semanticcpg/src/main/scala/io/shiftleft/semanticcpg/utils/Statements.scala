package io.shiftleft.semanticcpg.utils

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

object Statements {
  def countAll(cpg: Cpg): Long =
    cpg.method.topLevelExpressions.size
}
