package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

import java.io.{File => JFile}
import java.util.regex.Matcher
import scala.collection.mutable

/** Attempts to set the <code>methodFullName</code> and link to callees using the recovered type information from
  * [[PythonTypeRecovery]].
  *
  * @param cpg
  *   the target code property graph.
  */
class PythonTypeHintCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.call.where(_.callee(NoResolve))
  }


}
