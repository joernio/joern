package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Local}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import overflowdb.traversal.{NodeOps, Traversal}

import java.io.File
import java.nio.file.Paths

/** This pass enhances the call graph and call nodes by interpreting assignments of the form `foo = require("module")`.
  */
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
        if (path.endsWith(".mjs") || path.endsWith(".js")) {
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

    def relativeExport: String = fileToInclude.stripPrefix(dirHoldingModule + File.separator)

  }

  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.call("require").where(_.inAssignment.target).map(Require.apply(_)).foreach { require =>
      // Set types of locals on the LHS of a require
      require.call.method.ast.isIdentifier
        .filter(_.code.equals(require.target))
        .collectFirst(i => i.refsTo.collectAll[Local].headOption)
        .flatten
        .foreach { definingLocal =>
          (definingLocal.referencingIdentifiers ++ Seq(definingLocal)).foreach { refId =>
            diffGraph.setNodeProperty(
              refId,
              PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
              definingLocal.dynamicTypeHintFullName ++ Seq(s"<export>::${require.relativeExport}")
            )
          }
        }
      require.methodFullName.foreach { fullName =>
        cpg.method.fullNameExact(fullName).foreach { method =>
          require.nodesToPatch.foreach { node =>
            diffGraph.setNodeProperty(node, PropertyNames.METHOD_FULL_NAME, fullName)
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
