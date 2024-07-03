package io.joern.x2cpg.layers

import better.files.File
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

case class AstDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpAst {

  val overlayName = "dumpAst"

  val description = "Dump abstract syntax trees to out/"

  def defaultOpts: AstDumpOptions = AstDumpOptions("out")
}

class DumpAst(options: AstDumpOptions) extends LayerCreator {
  override val overlayName: String       = DumpAst.overlayName
  override val description: String       = DumpAst.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      val str = method.dotAst.head
      (File(options.outDir) / s"$i-ast.dot").write(str)
    }
  }

}
