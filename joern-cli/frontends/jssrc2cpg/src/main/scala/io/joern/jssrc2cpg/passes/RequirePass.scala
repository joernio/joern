package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{NodeOps, Traversal}

import java.nio.file.Paths

class RequirePass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private val codeRoot                = cpg.metaData.root.headOption.getOrElse("")
  private val moduleExportAssignments = cpg.assignment.where(_.target.codeExact("module.exports")).l
  private val fileNameToMethodFullName = Traversal(moduleExportAssignments).source.isMethodRef.flatMap { ref =>
    ref.file.name.headOption.map(x => (Paths.get(codeRoot, x).toAbsolutePath.normalize.toString -> ref.methodFullName))
  }.toMap

  case class Require(call: Call) {
    val dirHoldingModule: String = call.file.name.headOption
      .map(x => Paths.get(codeRoot, x).getParent.toAbsolutePath.normalize.toString)
      .getOrElse("")
    val fileToInclude: String = call
      .argument(1)
      .start
      .isLiteral
      .code
      .map(stripQuotes)
      .map { x => Paths.get(dirHoldingModule, x).toAbsolutePath.normalize.toString + ".js" }
      .headOption
      .getOrElse("")
    val methodFullName: Option[String] = fileNameToMethodFullName.get(fileToInclude)
  }

  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.call("require").map(Require).foreach { require =>
      require.methodFullName.foreach { fullName =>
        diffGraph.setNodeProperty(require.call, "METHOD_FULL_NAME", fullName)
        cpg.method.fullNameExact(fullName).foreach { method =>
          diffGraph.addEdge(require.call, method, EdgeTypes.CALL)
        }
      }
    }
  }

  private def stripQuotes(str: String) = {
    if (str.length >= 2 && str.startsWith("\"") && str.endsWith("\"")) {
      str.substring(1, str.length - 1)
    } else if (str.length >= 2 && str.startsWith("'") && str.endsWith("'")) {
      str.substring(1, str.length - 1)
    } else {
      str
    }
  }

}
