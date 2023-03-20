package io.joern.pysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecovery.isDummyType
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, MethodBase}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import scala.collection.mutable

class PythonTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Traversal[Call] = super.calls.nameNot("^(import).*")

  override def calleeNames(c: Call): Seq[String] = super.calleeNames(c).map {
    // Python call from  a type
    case typ if typ.split("\\.").lastOption.exists(_.charAt(0).isUpper) => s"$typ.${Defines.ConstructorMethodName}"
    // Python call from a function pointer
    case typ => typ
  }

  override def linkCalls(builder: DiffGraphBuilder): Unit = {
    val methodMap = mutable.HashMap.empty[String, MethodBase]
    val callerAndCallees = calls
      .map(call => (call, calleeNames(call)))
      .toList
    // Gather all method nodes and/or stubs
    callerAndCallees
      .foreach { case (call, methodNames) =>
        val ms = callees(methodNames).l
        if (ms.nonEmpty) {
          ms.foreach { m => methodMap.put(m.fullName, m) }
        } else {
          val mNames = ms.map(_.fullName).toSet
          methodNames
            .filterNot(mNames.contains)
            .map(fullName => createMethodStub(fullName, call, builder))
            .foreach { m => methodMap.put(m.fullName, m) }
        }
      }
    // Link edges to method nodes
    callerAndCallees.foreach { case (call, methodNames) =>
      methodNames
        .flatMap(methodMap.get)
        .foreach { m => builder.addEdge(call, m, EdgeTypes.CALL) }
      if (methodNames.size == 1) {
        builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, methodNames.head)
      } else if (methodNames.size > 1) {
        val nonDummyMethodNames = methodNames.filterNot(isDummyType)
        if (nonDummyMethodNames.nonEmpty) {
          builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, nonDummyMethodNames.minBy(_.length))
        }
      }
    }
  }

}
