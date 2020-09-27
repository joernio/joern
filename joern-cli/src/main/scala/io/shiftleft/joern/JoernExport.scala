package io.shiftleft.joern

import better.files.File
import better.files.Dsl._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.layers.dataflows.{Cpg14DumpOptions, DumpCpg14, OssDataFlow, OssDataFlowOptions}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.joern.console.JoernWorkspaceLoader
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

case class ExporterConfig(cpgFileName: String = "cpg.bin", outDir: String = "out")

object JoernExport extends App {

  private def parseConfig: Option[ExporterConfig] =
    new scopt.OptionParser[ExporterConfig](getClass.getSimpleName) {
      help("help")
      arg[String]("cpg")
        .text("CPG file name")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]("out")
        .text("output directory")
        .action((x, c) => c.copy(outDir = x))
    }.parse(args, ExporterConfig())

  parseConfig.foreach { config =>
    if (File(config.outDir).exists) {
      System.err.println(s"Output directory ${config.outDir} already exists. Bailing out.")
    } else {
      if (!File(config.cpgFileName).exists) {
        System.err.println(s"CPG at ${config.cpgFileName} does not exist. Bailing out.")
      } else {
        val cpg = CpgBasedTool.loadFromOdb(config.cpgFileName)
        addDataFlowOverlayIfNonExistent(cpg)
        val context = new LayerCreatorContext(cpg)

        mkdir(File(config.outDir))
        implicit val semantics: Semantics = JoernWorkspaceLoader.defaultSemantics
        if (semantics.elements.isEmpty) {
          System.err.println("Warning: semantics are empty.")
        }
        val opts = Cpg14DumpOptions(config.outDir)
        new DumpCpg14(opts).create(context)
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

}
