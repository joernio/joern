package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

/** Attempts to set the <code>methodFullName</code> and link to callees using the recovered type information from
  * [[XTypeRecovery]]. Note that some methods may not be present as they could be external and have been dynamically
  * discovered, thus the [[io.joern.x2cpg.passes.base.MethodStubCreator]] would have missed it.
  *
  * @param cpg
  *   the target code property graph.
  */
abstract class XTypeHintCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  implicit private val resolver: NoResolve.type = NoResolve

  def calls: Traversal[Call] = cpg.call
    .filterNot(c => c.name.startsWith("<operator>"))
    .filter(c => calleeNames(c).nonEmpty)
    .filter(_.callee.isEmpty)

  def calleeNames(c: Call): Seq[String] =
    (c.dynamicTypeHintFullName ++ Seq(c.typeFullName))
      .filterNot(_.equals("ANY"))
      .distinct

  private def callees(names: Seq[String]): Traversal[Method] = cpg.method.fullNameExact(names: _*)

  override def run(builder: DiffGraphBuilder) = linkCalls(builder)

  private def linkCalls(builder: DiffGraphBuilder): Unit =
    calls.map(call => (call, calleeNames(call))).foreach { case (call, ms) =>
      callees(ms).foreach { m => builder.addEdge(call, m, EdgeTypes.CALL) }
      if (ms.size == 1) builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, ms.head)
    }

}
