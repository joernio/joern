package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import java.util.regex.Pattern

class JavaTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override protected def calls: Traversal[Call] = {
    cpg.call
      .nameNot("<operator>.*", "<operators>.*")
      .filter(c =>
        calleeNames(c).nonEmpty && c.callee.fullNameNot(Pattern.quote(Defines.UnresolvedNamespace) + ".*").isEmpty
      )
  }

}
