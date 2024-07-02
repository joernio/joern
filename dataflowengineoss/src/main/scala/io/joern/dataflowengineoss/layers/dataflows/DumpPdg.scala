package io.joern.dataflowengineoss.layers.dataflows

import better.files.File
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

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
      val str = method.dotPdg.head
      (File(options.outDir) / s"$i-pdg.dot").write(str)
    }
  }
}
