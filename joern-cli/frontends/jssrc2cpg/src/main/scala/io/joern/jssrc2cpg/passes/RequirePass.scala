package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.passes.RequirePass.{JsExportPrefix, stripQuotes}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Local}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import overflowdb.traversal.{NodeOps, Traversal}

import java.io.File
import java.nio.file.Paths

/** This pass enhances the call graph and call nodes by interpreting assignments of the form `foo = require("module")`.
  */
class RequirePass(cpg: Cpg) extends CpgPass(cpg) {

  private val codeRoot = cpg.metaData.root.headOption.getOrElse("")

  /** A map from file names to method full names based on assignments to `module.exports`.
    */
  private val fileNameToMethodFullName: Map[String, String] = {
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
  private val exportTable: Map[(String, String), String] = {
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

    private val symbol: String = call.astParent.fieldAccess.fieldIdentifier.canonicalName.headOption.getOrElse("")

    private val dirHoldingModule: String =
      call.file.name.headOption
        .map(x => Paths.get(codeRoot, x).getParent.toAbsolutePath.normalize.toString)
        .getOrElse("")

    private val fileToInclude: String = call
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
          s"$path.js"
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

    val callsToPatch: List[Call] = call.file.method.ast.isCall.nameExact(target).dedup.l

    val variableToPatch: VariableInformation = VariableInformation(
      call.file.method.ast.isIdentifier
        .nameExact(target)
        .flatMap(i => Seq(i) ++ i.refsTo.collectAll[Local].toSeq)
        .dedup
        .toList
    )

    def relativeExport: String = fileToInclude.stripPrefix(s"$dirHoldingModule${File.separator}")

  }

  /** Represents a local and all of its reference identifiers.
    * @param nodes
    *   a list of a local and its reference identifiers.
    */
  case class VariableInformation(nodes: List[AstNode]) {

    /** Whether this variable has been set as immutable or not.
      */
    lazy val isImmutable: Boolean = nodes.exists(_.inAssignment.code.exists(_.startsWith("const ")))

    /** The dynamic type hints attached to this variable. This assumes all nodes have the same property value at this
      * time.
      */
    lazy val dynamicTypeHintFullName: Seq[String] = nodes.headOption.map(
      _.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, IndexedSeq.empty[String])
    ).getOrElse(IndexedSeq.empty[String])
  }

  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    val requires = cpg.imports.call.dedup
      .where(_.inAssignment.target)
      .map(Require.apply(_))
      .toSeq
    // Set type hints of affected locals and identifiers
    requires
      .groupBy(require => require.variableToPatch)
      .foreach { case (varInfo: VariableInformation, rs: Seq[Require]) =>
        varInfo.nodes.foreach { n =>
          diffGraph.setNodeProperty(
            n.node,
            if (varInfo.isImmutable) PropertyNames.TYPE_FULL_NAME else PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
            if (varInfo.isImmutable) rs.map(x => s"$JsExportPrefix${x.relativeExport}").headOption.getOrElse("ANY")
            else varInfo.dynamicTypeHintFullName ++ rs.map(x => s"$JsExportPrefix${x.relativeExport}")
          )
        }
      }
    // Set method full names of calls to patch
    requires.foreach { require =>
      require.methodFullName.foreach { fullName =>
        cpg.method.fullNameExact(fullName).foreach { method =>
          require.callsToPatch.foreach { node =>
            diffGraph.setNodeProperty(node, PropertyNames.METHOD_FULL_NAME, fullName)
            diffGraph.addEdge(node, method, EdgeTypes.CALL)
          }
        }
      }
    }
  }

}

object RequirePass {
  val JsExportPrefix: String = "<export>::"

  def stripQuotes(str: String): String = {
    if (str.length >= 2 && str.startsWith("\"") && str.endsWith("\"")) {
      str.substring(1, str.length - 1)
    } else if (str.length >= 2 && str.startsWith("'") && str.endsWith("'")) {
      str.substring(1, str.length - 1)
    } else {
      str
    }
  }

}
