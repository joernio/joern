package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class JavaScriptTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override protected val pathSep = ':'

  override protected def calls: Iterator[Call] = cpg.call
    .or(_.nameNot("<operator>.*", "<operators>.*"), _.name("<operator>.new"))
    .filter(c => calleeNames(c).nonEmpty && c.callee.isEmpty)

}
