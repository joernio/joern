package io.joern.x2cpg.layers

import better.files.File
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

case class AllDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpAll {
  val overlayName = "dumpAll"

  val description = "Dump entire graph to out/"

  def defaultOpts: AllDumpOptions = AllDumpOptions("out")
}

class DumpAll(options: AllDumpOptions) extends LayerCreator {
  override val overlayName: String       = DumpAll.overlayName
  override val description: String       = DumpAll.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext): Unit = {
    val cpg       = context.cpg
    val dotGraph  = DotSerializer.Graph(vertices = cpg.all, edges = cpg.graph.allEdges.map(DotSerializer.Edge))
    val dotString = DotSerializer.dotGraph(graph = dotGraph)
    (File(options.outDir) / s"all.dot").write(dotString)
  }

}
