package io.joern.joerncli

import better.files.Dsl._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.joerncli.JoernExport.Representations
import io.joern.joerncli.console.JoernWorkspaceLoader
import io.joern.x2cpg.layers._
import io.shiftleft.semanticcpg.layers._
import overflowdb.formats.ExportResult
import overflowdb.formats.neo4jcsv.Neo4jCsvExporter

import scala.util.Using

case class ExporterConfig(
  cpgFileName: String = "cpg.bin",
  outDir: String = "out",
  repr: Representations.Value = Representations.cpg14
)

object JoernExport extends App {

  object Representations extends Enumeration {
    val ast, cfg, ddg, cdg, pdg, cpg14, neo4jcsv = Value
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
        .text(s"representation to extract: [${Representations.values.toSeq.sorted.mkString("|")}]")
        .action((x, c) => c.copy(repr = Representations.withName(x)))
    }.parse(args, ExporterConfig())

  parseConfig.foreach { config =>
    if (File(config.outDir).exists) {
      System.err.println(s"Output directory ${config.outDir} already exists. Bailing out.")
    } else {
      if (!File(config.cpgFileName).exists) {
        System.err.println(s"CPG at ${config.cpgFileName} does not exist. Bailing out.")
      } else {
        Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
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
            case Representations.neo4jcsv =>
              val ExportResult(nodeCount, edgeCount, files, additionalInfo) =
                Neo4jCsvExporter.runExport(cpg.graph, config.outDir)
              println(s"export completed successfully: $nodeCount nodes, $edgeCount edges in ${files.size} files")
              println(additionalInfo)
            case repr =>
              System.err.println(s"unknown representation: $repr. Baling out.")
          }
        }
      }
    }
  }

}
