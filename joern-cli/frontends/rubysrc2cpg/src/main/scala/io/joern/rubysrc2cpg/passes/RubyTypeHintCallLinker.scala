package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.Traversal
import io.shiftleft.semanticcpg.language._

import java.util.regex.Pattern

class RubyTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Traversal[Call] = super.calls.nameNot("^(require).*")

}
