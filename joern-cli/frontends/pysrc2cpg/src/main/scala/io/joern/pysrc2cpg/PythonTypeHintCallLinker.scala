package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import overflowdb.traversal.Traversal

class PythonTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Traversal[Call] = super.calls.filterNot(c => c.name.matches("^(import).*"))

  override def calleeNames(c: Call): Seq[String] =
    super
      .calleeNames(c)
      .map {
        // Python call from  a type
        case typ if typ.split("\\.").lastOption.exists(_.charAt(0).isUpper) => s"$typ.${c.name}"
        // Python call from a function pointer
        case typ => typ
      }

}
