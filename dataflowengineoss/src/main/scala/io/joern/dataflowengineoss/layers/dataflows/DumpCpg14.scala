package io.joern.dataflowengineoss.layers.dataflows

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

import java.nio.file.{Files, Paths}

case class Cpg14DumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpCpg14 {

  val overlayName = "dumpCpg14"

  val description = "Dump Code Property Graph (2014) to out/"

  def defaultOpts: Cpg14DumpOptions = Cpg14DumpOptions("out")
}

class DumpCpg14(options: Cpg14DumpOptions)(implicit semantics: Semantics = DefaultSemantics()) extends LayerCreator {
  override val overlayName: String       = DumpDdg.overlayName
  override val description: String       = DumpDdg.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str        = method.dotCpg14.head
      val outputPath = Paths.get(options.outDir) / s"$i-cpg.dot"
      Files.writeString(outputPath, str)
    }
  }
}
