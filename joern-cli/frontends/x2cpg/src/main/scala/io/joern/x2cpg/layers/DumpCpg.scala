package io.joern.x2cpg.layers

import better.files.File
import flatgraph.{Accessors, Edge, GNode}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

import scala.collection.mutable

case class CpgDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpCpg {
  val overlayName = "dumpCpg"
  val description = "Dump entire cpg, split by methods, to out/"
  def defaultOpts: CpgDumpOptions = CpgDumpOptions("out")
}

class DumpCpg(options: CpgDumpOptions) extends LayerCreator {
  override val overlayName: String       = DumpCpg.overlayName
  override val description: String       = DumpCpg.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg

    if (cpg.method.isEmpty) {
      (File(options.outDir) / "cpg.dot").write("digraph {}")
    } else {
      val windowsFilenameDeduplicationHelper = mutable.Set.empty[String]
      splitByMethod(cpg).foreach { case subGraph@MethodSubGraph(methodName, methodFilename, nodes) =>
        val relativeFilename = sanitizedFileName(
          methodName,
          methodFilename,
          "dot",
          windowsFilenameDeduplicationHelper
        )
        val dotGraph  = DotSerializer.Graph(vertices = nodes.cast[StoredNode], edges = subGraph.edges.map(DotSerializer.Edge))
        val dotString = DotSerializer.dotGraph(graph = dotGraph)
        println(s"XXX4 $relativeFilename")
        val outFile = File(options.outDir) / relativeFilename
        outFile.parent.createDirectoryIfNotExists(createParents = true)
        outFile.write(dotString)
      }
    }
  }

  /** for each method in the cpg: recursively traverse all AST edges to get the subgraph of nodes within this method add
   * the method and this subgraph to the export add all edges between all of these nodes to the export
   */
  private def splitByMethod(cpg: Cpg): IterableOnce[MethodSubGraph] = {
    cpg.method.map { method =>
      MethodSubGraph(methodName = method.name, methodFilename = method.filename, nodes = method.ast.toSet)
    }
  }

  case class MethodSubGraph(methodName: String, methodFilename: String, nodes: Set[GNode]) {
    def edges: Set[Edge] = {
      for {
        node <- nodes
        edge <- Accessors.getEdgesOut(node)
        if nodes.contains(edge.dst)
      } yield edge
    }
  }

  /** @param windowsFilenameDeduplicationHelper
   *   utility map to ensure we don't override output files for identical method names
   */
  private def sanitizedFileName(
                                 methodName: String,
                                 methodFilename: String,
                                 fileExtension: String,
                                 windowsFilenameDeduplicationHelper: mutable.Set[String]
                               ): String = {
    val sanitizedMethodName = methodName.replaceAll("[^a-zA-Z0-9-_\\.]", "_")
    val sanitizedFilename =
      if (scala.util.Properties.isWin) {
        // windows has some quirks in it's file system, e.g. we need to ensure paths aren't too long - so we're using a
        // different strategy to sanitize windows file names: first occurrence of a given method uses the method name
        // any methods with the same name afterwards get a `_` suffix
        if (windowsFilenameDeduplicationHelper.contains(sanitizedMethodName)) {
          sanitizedFileName(s"${methodName}_", methodFilename, fileExtension, windowsFilenameDeduplicationHelper)
        } else {
          windowsFilenameDeduplicationHelper.add(sanitizedMethodName)
          sanitizedMethodName
        }
      } else { // non-windows
        // handle leading `/` to ensure we're not writing outside of the output directory
        val sanitizedPath =
          if (methodFilename.startsWith("/")) s"_root_/$methodFilename"
          else methodFilename
        s"$sanitizedPath/$sanitizedMethodName"
      }

    s"$sanitizedFilename.$fileExtension"
  }
}
