package io.joern.dataflowengineoss.layers.dataflows

import better.files.File
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

case class PdgDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpPdg {

  val overlayName = "dumpPdg"

  val description = "Dump program dependence graph to out/"

  def defaultOpts: PdgDumpOptions = PdgDumpOptions("out")
}

class DumpPdg(options: PdgDumpOptions)(implicit semantics: Semantics) extends LayerCreator {
  override val overlayName: String  = DumpPdg.overlayName
  override val description: String  = DumpPdg.description
  override val modifiesCpg: Boolean = false

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str = method.dotPdg.head
      (File(options.outDir) / s"${i}-pdg.dot").write(str)
    }
  }
}
