package io.joern.x2cpg.layers

import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.{Files, Path, Paths}

case class CdgDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpCdg {

  val overlayName = "dumpCdg"

  val description = "Dump control dependence graph to out/"

  def defaultOpts: CdgDumpOptions = CdgDumpOptions("out")
}

class DumpCdg(options: CdgDumpOptions) extends LayerCreator {
  override val overlayName: String       = DumpCdg.overlayName
  override val description: String       = DumpCdg.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str        = method.dotCdg.head
      val outputPath = Paths.get(options.outDir) / s"$i-cdg.dot"
      Files.writeString(outputPath, str)
    }
  }
}
