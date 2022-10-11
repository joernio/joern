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

  private val codeRoot = cpg.metaData.root.headOption.getOrElse("")

  /** A map from file names to method full names based on assignments to `module.exports`.
    */
  val fileNameToMethodFullName: Map[String, String] = {
    val moduleExportAssignments = cpg.assignment.where(_.target.codeExact("module.exports")).l
    Traversal(moduleExportAssignments).source.isMethodRef.flatMap { ref =>
      ref.file.name.headOption.map { x =>
        val filename = fileToAbsolutePath(x)
        filename -> ref.methodFullName
      }
    }.toMap
  }

  /** A map from (filename, symbol) pairs to method full names based on `export` statements.
    */
  val exportTable: Map[(String, String), String] = {
    val assignments = cpg.methodRef
      .where(_.method.fullName(".*::program"))
      .inAssignment
    val pairs =
      for {
        assignment     <- assignments
        identifier     <- Traversal(assignment).target.isIdentifier.name.l
        filename       <- Traversal(assignment).file.name.map(fileToAbsolutePath).l
        methodFullName <- Traversal(assignment).source.isMethodRef.methodFullName.l
      } yield (filename, identifier) -> methodFullName
    pairs.toMap
  }

  private def fileToAbsolutePath(file: String) =
    Paths.get(codeRoot, file).toAbsolutePath.normalize.toString

  case class Require(call: Call) {

    val symbol: String = call.astParent.fieldAccess.fieldIdentifier.canonicalName.headOption.getOrElse("")

    val dirHoldingModule: String = call.file.name.headOption
      .map(x => Paths.get(codeRoot, x).getParent.toAbsolutePath.normalize.toString)
      .getOrElse("")

    val fileToInclude: String = call
      .argument(1)
      .start
      .isLiteral
      .code
      .map(stripQuotes)
      .map { x =>
        val path = Paths.get(dirHoldingModule, x).toAbsolutePath.normalize.toString
        if (path.endsWith(".mjs")) {
          path
        } else {
          path + ".js"
        }
      }
      .headOption
      .getOrElse("")

    val methodFullName: Option[String] = {
      if (symbol == "") {
        fileNameToMethodFullName.get(fileToInclude)
      } else {
        exportTable.get((fileToInclude, symbol))
      }
    }

    val target: String = call.inAssignment.target.code.head

    val nodesToPatch: List[Call] = call.file.method.ast.isCall.nameExact(target).dedup.l

  }

  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.call("require").where(_.inAssignment.target).map(Require.apply(_)).foreach { require =>
      require.methodFullName.foreach { fullName =>
        cpg.method.fullNameExact(fullName).foreach { method =>
          require.nodesToPatch.foreach { node =>
            diffGraph.setNodeProperty(node, "METHOD_FULL_NAME", fullName)
            diffGraph.addEdge(node, method, EdgeTypes.CALL)
          }
        }
      }
    }
  }

  private def stripQuotes(str: String): String = {
    if (str.length >= 2 && str.startsWith("\"") && str.endsWith("\"")) {
      str.substring(1, str.length - 1)
    } else if (str.length >= 2 && str.startsWith("'") && str.endsWith("'")) {
      str.substring(1, str.length - 1)
    } else {
      str
    }
  }

}
