package io.joern.rubysrc2cpg.deprecated.passes

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class RubyTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Iterator[Call] = super.calls.nameNot("^(require).*")

}
