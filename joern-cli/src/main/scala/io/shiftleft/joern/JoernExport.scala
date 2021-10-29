package io.shiftleft.joern

import better.files.File
import better.files.Dsl._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengineoss.layers.dataflows.{
  Cpg14DumpOptions,
  DdgDumpOptions,
  DumpCpg14,
  DumpDdg,
  DumpPdg,
  OssDataFlow,
  OssDataFlowOptions,
  PdgDumpOptions
}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.joern.console.JoernWorkspaceLoader
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{
  AstDumpOptions,
  CdgDumpOptions,
  CfgDumpOptions,
  DumpAst,
  DumpCdg,
  DumpCfg,
  LayerCreatorContext
}

case class ExporterConfig(cpgFileName: String = "cpg.bin", outDir: String = "out", repr: String = "cpg14")

object JoernExport extends App {

  object Representations {
    val ast = "ast"
    val cfg = "cfg"
    val ddg = "ddg"
    val cdg = "cdg"
    val pdg = "pdg"
    val cpg14 = "cpg14"
  }

  private def parseConfig: Option[ExporterConfig] =
    new scopt.OptionParser[ExporterConfig]("joern-export") {
      head("Dump intermediate graph representations of code onto disk")
      help("help")
      arg[String]("cpg")
        .text("CPG file name ('cpg.bin' by default)")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]("out")
        .text("output directory")
        .action((x, c) => c.copy(outDir = x))
      opt[String]("repr")
        .text("representation to extract: [ast|cfg|ddg|cdg|pdg|cpg14]")
        .action((x, c) => c.copy(repr = x))
    }.parse(args, ExporterConfig())

  parseConfig.foreach { config =>
    if (File(config.outDir).exists) {
      System.err.println(s"Output directory ${config.outDir} already exists. Bailing out.")
    } else {
      if (!File(config.cpgFileName).exists) {
        System.err.println(s"CPG at ${config.cpgFileName} does not exist. Bailing out.")
      } else {
        val cpg = CpgBasedTool.loadFromOdb(config.cpgFileName)
        CpgBasedTool.addDataFlowOverlayIfNonExistent(cpg)
        val context = new LayerCreatorContext(cpg)

        mkdir(File(config.outDir))
        implicit val semantics: Semantics = JoernWorkspaceLoader.defaultSemantics
        if (semantics.elements.isEmpty) {
          System.err.println("Warning: semantics are empty.")
        }

        config.repr match {
          case Representations.ast =>
            new DumpAst(AstDumpOptions(config.outDir)).create(context)
          case Representations.cfg =>
            new DumpCfg(CfgDumpOptions(config.outDir)).create(context)
          case Representations.ddg =>
            new DumpDdg(DdgDumpOptions(config.outDir)).create(context)
          case Representations.cdg =>
            new DumpCdg(CdgDumpOptions(config.outDir)).create(context)
          case Representations.pdg =>
            new DumpPdg(PdgDumpOptions(config.outDir)).create(context)
          case Representations.cpg14 =>
            new DumpCpg14(Cpg14DumpOptions(config.outDir)).create(context)
          case repr =>
            System.err.println(s"unknown representation: $repr. Baling out.")
        }
        cpg.close()
      }
    }
  }

}
