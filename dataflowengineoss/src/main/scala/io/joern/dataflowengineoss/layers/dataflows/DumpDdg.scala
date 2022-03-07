package io.joern.dataflowengineoss.layers.dataflows

import better.files.File
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

case class DdgDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpDdg {

  val overlayName = "dumpDdg"

  val description = "Dump data dependence graphs to out/"

  def defaultOpts: DdgDumpOptions = DdgDumpOptions("out")
}

class DumpDdg(options: DdgDumpOptions)(implicit semantics: Semantics) extends LayerCreator {
  override val overlayName: String  = DumpDdg.overlayName
  override val description: String  = DumpDdg.description
  override val modifiesCpg: Boolean = false

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str = method.dotDdg.head
      (File(options.outDir) / s"${i}-ddg.dot").write(str)
    }
  }
}
