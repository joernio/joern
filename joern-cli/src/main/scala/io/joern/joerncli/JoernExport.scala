package io.joern.joerncli

import better.files.Dsl._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.joerncli.console.JoernWorkspaceLoader
import io.joern.x2cpg.layers._
import io.shiftleft.semanticcpg.layers._
import overflowdb.Graph
import overflowdb.formats.ExportResult
import overflowdb.formats.dot.DotExporter
import overflowdb.formats.graphml.GraphMLExporter
import overflowdb.formats.neo4jcsv.Neo4jCsvExporter

import scala.util.Using

object JoernExport extends App {

  case class Config(
    cpgFileName: String = "cpg.bin",
    outDir: String = "out",
    repr: Representation.Value = Representation.cpg14,
    format: Format.Value = Format.dot
  )

  /** Choose from either a subset of the graph, or the entire graph (all).
    */
  object Representation extends Enumeration {
    val ast, cfg, ddg, cdg, pdg, cpg14, all = Value
  }
  object Format extends Enumeration {
    val dot, neo4jcsv, graphml = Value
  }

  private def parseConfig: Option[Config] =
    new scopt.OptionParser[Config]("joern-export") {
      head("Dump intermediate graph representations (or entire graph) of code in a given export format")
      help("help")
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]('o', "out")
        .text("output directory - will be created and must not yet exist")
        .action((x, c) => c.copy(outDir = x))
      opt[String]("repr")
        .text(
          s"representation to extract: [${Representation.values.toSeq.sorted.mkString("|")}] - defaults to `${Representation.cpg14}`"
        )
        .action((x, c) => c.copy(repr = Representation.withName(x)))
      opt[String]("format")
        .action((x, c) => c.copy(format = Format.withName(x)))
        .text(s"export format, one of [${Format.values.toSeq.sorted.mkString("|")}] - defaults to `${Format.dot}`")
    }.parse(args, Config())

  parseConfig.foreach { config =>
    if (File(config.outDir).exists)
      exitWithError(s"Output directory `${config.outDir}` already exists.")
    if (File(config.cpgFileName).notExists)
      exitWithError(s"CPG at ${config.cpgFileName} does not exist.")

    Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
      CpgBasedTool.addDataFlowOverlayIfNonExistent(cpg)
      val context = new LayerCreatorContext(cpg)

      mkdir(File(config.outDir))
      implicit val semantics: Semantics = JoernWorkspaceLoader.defaultSemantics
      if (semantics.elements.isEmpty) {
        System.err.println("Warning: semantics are empty.")
      }

      (config.repr, config.format) match {
        case (Representation.ast, Format.dot) =>
          new DumpAst(AstDumpOptions(config.outDir)).create(context)
        case (Representation.cfg, Format.dot) =>
          new DumpCfg(CfgDumpOptions(config.outDir)).create(context)
        case (Representation.ddg, Format.dot) =>
          new DumpDdg(DdgDumpOptions(config.outDir)).create(context)
        case (Representation.cdg, Format.dot) =>
          new DumpCdg(CdgDumpOptions(config.outDir)).create(context)
        case (Representation.pdg, Format.dot) =>
          new DumpPdg(PdgDumpOptions(config.outDir)).create(context)
        case (Representation.cpg14, Format.dot) =>
          new DumpCpg14(Cpg14DumpOptions(config.outDir)).create(context)
        case (Representation.all, Format.neo4jcsv) =>
          overflowdbExport(cpg.graph, config.outDir, Neo4jCsvExporter)
        case (Representation.all, Format.graphml) =>
          overflowdbExport(cpg.graph, config.outDir, GraphMLExporter)
        case (Representation.all, Format.dot) =>
          overflowdbExport(cpg.graph, config.outDir, DotExporter)
        case (repr, format) =>
          exitWithError(s"combination of repr=$repr and format=$format not (yet) supported")
      }
    }
  }

  private def overflowdbExport(graph: Graph, outDir: String, exporter: overflowdb.formats.Exporter): Unit = {
    val ExportResult(nodeCount, edgeCount, files, additionalInfo) = exporter.runExport(graph, outDir)
    println(s"export completed successfully: $nodeCount nodes, $edgeCount edges in ${files.size} files")
    additionalInfo.foreach(println)
  }

  private def exitWithError(msg: String): Unit = {
    System.err.println(s"error: $msg")
    System.exit(1)
  }

}
