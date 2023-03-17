package io.joern.pysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class PythonTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Traversal[Call] = super.calls.nameNot("^(import).*")

  override def calleeNames(c: Call): Seq[String] = super.calleeNames(c).map {
    // Python call from  a type
    case typ if typ.split("\\.").lastOption.exists(_.charAt(0).isUpper) => s"$typ.${Defines.ConstructorMethodName}"
    // Python call from a function pointer
    case typ => typ
  }

}
