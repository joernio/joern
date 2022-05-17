package io.joern.joerncli

import better.files.Dsl._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.joerncli.JoernExport.{Format, Representation}
import io.joern.joerncli.console.JoernWorkspaceLoader
import io.joern.x2cpg.layers._
import io.shiftleft.semanticcpg.layers._
import overflowdb.formats.ExportResult
import overflowdb.formats.neo4jcsv.Neo4jCsvExporter

import scala.util.Using

object JoernExport extends App {

  case class Config(
                     cpgFileName: String = "cpg.bin",
                     outDir: String = "out",
                     repr: Representation.Value = Representation.cpg14,
                     format: Format.Value = Format.dot
   )

  /**
    * Choose from either a subset of the graph, or the entire graph (all).
    */
  object Representation extends Enumeration {
    val ast, cfg, ddg, cdg, pdg, cpg14, all = Value
  }
  object Format extends Enumeration {
    val dot, neo4jcsv = Value
  }

  private def parseConfig: Option[Config] =
    new scopt.OptionParser[Config]("joern-export") {
      head("Dump intermediate graph representations (or entire graph) of code in a given export format")
      help("help")
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]("out")
        .text("output directory - will be created and must not yet exist")
        .action((x, c) => c.copy(outDir = x))
      opt[String]("repr")
        .text(s"representation to extract: [${Representation.values.toSeq.sorted.mkString("|")}] - defaults to ${Representation.cpg14}")
        .action((x, c) => c.copy(repr = Representation.withName(x)))
      opt[String]("format")
        .required()
        .action((x, c) => c.copy(format = Format.withName(x)))
        .text(s"export format, one of [${Format.values.toSeq.sorted.mkString("|")}] - defaults to ${Format.dot}")
    }.parse(args, Config())

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
            case Representation.ast =>
              new DumpAst(AstDumpOptions(config.outDir)).create(context)
            case Representation.cfg =>
              new DumpCfg(CfgDumpOptions(config.outDir)).create(context)
            case Representation.ddg =>
              new DumpDdg(DdgDumpOptions(config.outDir)).create(context)
            case Representation.cdg =>
              new DumpCdg(CdgDumpOptions(config.outDir)).create(context)
            case Representation.pdg =>
              new DumpPdg(PdgDumpOptions(config.outDir)).create(context)
            case Representation.cpg14 =>
              new DumpCpg14(Cpg14DumpOptions(config.outDir)).create(context)
            case Representation.neo4jcsv =>
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
