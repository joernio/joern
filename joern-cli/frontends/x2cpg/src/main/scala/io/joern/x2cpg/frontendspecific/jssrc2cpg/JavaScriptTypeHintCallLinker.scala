package io.joern.x2cpg.frontendspecific.jssrc2cpg

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class JavaScriptTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override protected val pathSep = ":"

  override protected def calls: Iterator[Call] = cpg.call
    .or(_.nameNot("<operator>.*", "<operators>.*"), _.name(Defines.OperatorsNew))
    .filter(c => calleeNames(c).nonEmpty && c.callee.isEmpty)

}
