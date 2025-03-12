package io.joern.dataflowengineoss.layers.dataflows

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Files, Paths}

case class PdgDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpPdg {

  val overlayName = "dumpPdg"

  val description = "Dump program dependence graph to out/"

  def defaultOpts: PdgDumpOptions = PdgDumpOptions("out")
}

class DumpPdg(options: PdgDumpOptions)(implicit semantics: Semantics = DefaultSemantics()) extends LayerCreator {
  override val overlayName: String       = DumpPdg.overlayName
  override val description: String       = DumpPdg.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str        = method.dotPdg.head
      val outputPath = Paths.get(options.outDir) / s"$i-pdg.dot"
      Files.writeString(outputPath, str)
    }
  }
}
