package io.joern.x2cpg.layers

import better.files.File
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

case class CdgDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpCdg {

  val overlayName = "dumpCdg"

  val description = "Dump control dependence graph to out/"

  def defaultOpts: CdgDumpOptions = CdgDumpOptions("out")
}

class DumpCdg(options: CdgDumpOptions) extends LayerCreator {
  override val overlayName: String  = DumpCdg.overlayName
  override val description: String  = DumpCdg.description
  override val modifiesCpg: Boolean = false

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str = method.dotCdg.head
      (File(options.outDir) / s"${i}-cdg.dot").write(str)
    }
  }
}
