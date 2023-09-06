package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecovery.isDummyType
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class PythonTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Iterator[Call] = super.calls.nameNot("^(import).*")

  override def calleeNames(c: Call): Seq[String] = super.calleeNames(c).map {
    // Python call from  a type
    case typ if typ.split("\\.").lastOption.exists(_.charAt(0).isUpper) => s"$typ.__init__"
    // Python call from a function pointer
    case typ => typ
  }

  override def setCallees(call: Call, methodNames: Seq[String], builder: DiffGraphBuilder): Unit = {
    if (methodNames.sizeIs == 1) {
      super.setCallees(call, methodNames, builder)
    } else if (methodNames.sizeIs > 1) {
      val nonDummyMethodNames =
        methodNames.filterNot(x => isDummyType(x) || x.startsWith(PythonAstVisitor.builtinPrefix + "None"))
      super.setCallees(call, nonDummyMethodNames, builder)
    }
  }

}
