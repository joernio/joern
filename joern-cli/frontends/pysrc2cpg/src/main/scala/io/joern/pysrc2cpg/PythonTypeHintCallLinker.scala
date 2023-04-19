package io.joern.pysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecovery.isDummyType
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, MethodBase}
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

  override protected def linkCallsToCallees(
    callerAndCallees: List[(Call, Seq[String])],
    methodMap: Map[String, MethodBase],
    builder: DiffGraphBuilder
  ): Unit = {
    // Link edges to method nodes
    callerAndCallees.foreach { case (call, methodNames) =>
      methodNames
        .flatMap(methodMap.get)
        .foreach { m => builder.addEdge(call, m, EdgeTypes.CALL) }
      if (methodNames.sizeIs == 1) {
        setCallees(call, methodNames, builder)
      } else if (methodNames.sizeIs > 1) {
        val nonDummyMethodNames =
          methodNames.filterNot(x => isDummyType(x) || x.startsWith(PythonAstVisitor.builtinPrefix + "None"))
        setCallees(call, nonDummyMethodNames, builder)
      }
    }
  }

}
