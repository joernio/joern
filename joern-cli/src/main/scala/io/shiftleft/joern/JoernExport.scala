package io.shiftleft.joern

import better.files.File
import better.files.Dsl._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{AstDumpOptions, DumpAst, LayerCreatorContext}

case class ExporterConfig(cpgFileName: String = "cpg.bin", outDir: String = "out")

object JoernExport extends App {

  parseConfig.foreach { config =>
    if (!File(config.cpgFileName).exists) {
      System.err.println(s"CPG at ${config.cpgFileName} does not exist. Bailing out.")
    } else {
      val cpg = CpgBasedTool.loadFromOdb(config.cpgFileName)
      addDataFlowOverlayIfNonExistent(cpg)
      val context = new LayerCreatorContext(cpg)
      if (File(config.outDir).exists) {
        System.err.println(s"Output directory ${config.outDir} already exists. Bailing out.")
      } else {
        mkdir(File(config.outDir))
        val opts = AstDumpOptions(config.outDir)
        new DumpAst(opts).create(context)
      }
    }
  }

  private def addDataFlowOverlayIfNonExistent(cpg: Cpg): Unit = {
    if (!cpg.metaData.overlays.exists(_ == OssDataFlow.overlayName)) {
      System.err.println("CPG does not have dataflow overlay. Calculating.")
      val opts = new OssDataFlowOptions()
      val context = new LayerCreatorContext(cpg)
      new OssDataFlow(opts).run(context)
    }
  }

  private def parseConfig: Option[ExporterConfig] =
    new scopt.OptionParser[ExporterConfig](getClass.getSimpleName) {
      help("help")
      arg[String]("cpg")
        .text("CPG file name")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]("out")
        .text("output directory")
        .action((x, c) => c.copy(cpgFileName = x))
    }.parse(args, ExporterConfig())

}
