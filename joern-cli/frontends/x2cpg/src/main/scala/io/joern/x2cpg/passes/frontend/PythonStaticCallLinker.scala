package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._

/** Attempts to set the <code>methodFullName</code> and <code>dispatchType</code> properties of "static" calls.
  * @param cpg
  *   the target code property graph.
  */
class PythonStaticCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit =
    cpg.method.where(_.nameExact("<module>")).foreach(module => runOnModule(module, builder))

  def runOnModule(module: Method, builder: DiffGraphBuilder): Unit = {
    // TODO
  }

}
