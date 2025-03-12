package io.joern.x2cpg.frontendspecific.php2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.Call

class PhpTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override protected val pathSep = "->"

  override protected def calls: Iterator[Call] = {
    cpg.call
      .nameNot("<operator>.*", "<operators>.*")
      .filter(c => calleeNames(c).nonEmpty && c.callee.forall(_.fullName.startsWith(Defines.UnresolvedNamespace)))
  }
}
