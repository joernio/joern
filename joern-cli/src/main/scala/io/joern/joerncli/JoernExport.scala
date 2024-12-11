package io.joern.joerncli

import better.files.Dsl.*
import better.files.File
import flatgraph.{Accessors, Edge, GNode}
import flatgraph.formats.ExportResult
import flatgraph.formats.dot.DotExporter
import flatgraph.formats.graphml.GraphMLExporter
import flatgraph.formats.graphson.GraphSONExporter
import flatgraph.formats.neo4jcsv.Neo4jCsvExporter
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows.*
import io.joern.dataflowengineoss.semanticsloader.{NoSemantics, Semantics}
import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.joern.x2cpg.layers.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.*

import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.util.Using

object JoernExport {

  case class Config(
    cpgFileName: String = "cpg.bin",
    outDir: String = "out",
    repr: Representation.Value = Representation.Cpg14,
    format: Format.Value = Format.Dot
  )

  /** Choose from either a subset of the graph, or the entire graph (all).
    */
  object Representation extends Enumeration {
    val Ast, Cfg, Ddg, Cdg, Pdg, Cpg14, Cpg, All = Value

    lazy val byNameLowercase: Map[String, Value] =
      values.map { value =>
        value.toString.toLowerCase -> value
      }.toMap

    def withNameIgnoreCase(s: String): Value =
      byNameLowercase.getOrElse(s, throw new NoSuchElementException(s"No value found for '$s'"))

  }
  object Format extends Enumeration {
    val Dot, Neo4jCsv, Graphml, Graphson = Value

    lazy val byNameLowercase: Map[String, Value] =
      values.map { value =>
        value.toString.toLowerCase -> value
      }.toMap

    def withNameIgnoreCase(s: String): Value =
      byNameLowercase.getOrElse(s, throw new NoSuchElementException(s"No value found for '$s'"))
  }

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      val outDir = config.outDir
      exitIfInvalid(outDir, config.cpgFileName)
      mkdir(File(outDir))

      Using.resource(CpgBasedTool.loadFromFile(config.cpgFileName)) { cpg =>
        exportCpg(cpg, config.repr, config.format, Paths.get(outDir).toAbsolutePath)
      }
    }
  }

  private def parseConfig(args: Array[String]): Option[Config] = {
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
          s"representation to extract: [${Representation.values.toSeq.map(_.toString.toLowerCase).sorted.mkString("|")}] - defaults to `${Representation.Cpg14}`"
        )
        .action((x, c) => c.copy(repr = Representation.withNameIgnoreCase(x)))
      opt[String]("format")
        .action((x, c) => c.copy(format = Format.withNameIgnoreCase(x)))
        .text(
          s"export format, one of [${Format.values.toSeq.map(_.toString.toLowerCase).sorted.mkString("|")}] - defaults to `${Format.Dot}`"
        )
    }.parse(args, Config())
  }

  def exportCpg(cpg: Cpg, representation: Representation.Value, format: Format.Value, outDir: Path): Unit = {
    implicit val semantics: Semantics = DefaultSemantics()
    if (semantics == NoSemantics) {
      System.err.println("Warning: semantics are empty.")
    }

    CpgBasedTool.addDataFlowOverlayIfNonExistent(cpg)
    val context = new LayerCreatorContext(cpg)

    format match {
      case Format.Dot =>
        exportDot(representation, outDir, context)
      case Format.Neo4jCsv =>
        exportWithFlatgraphFormat(cpg, representation, outDir, Neo4jCsvExporter)
      case Format.Graphml =>
        exportWithFlatgraphFormat(cpg, representation, outDir, GraphMLExporter)
      case Format.Graphson =>
        exportWithFlatgraphFormat(cpg, representation, outDir, GraphSONExporter)
    }
  }

  private def exportDot(repr: Representation.Value, outDir: Path, context: LayerCreatorContext): Unit = {
    val outDirStr = outDir.toString
    repr match {
      case Representation.Ast   => DumpAst(AstDumpOptions(outDirStr)).create(context)
      case Representation.Cfg   => DumpCfg(CfgDumpOptions(outDirStr)).create(context)
      case Representation.Ddg   => DumpDdg(DdgDumpOptions(outDirStr)).create(context)
      case Representation.Cdg   => DumpCdg(CdgDumpOptions(outDirStr)).create(context)
      case Representation.Pdg   => DumpPdg(PdgDumpOptions(outDirStr)).create(context)
      case Representation.Cpg14 => DumpCpg14(Cpg14DumpOptions(outDirStr)).create(context)
      case Representation.All   => DumpAll(AllDumpOptions(outDirStr)).create(context)
      case Representation.Cpg   => DumpCpg(CpgDumpOptions(outDirStr)).create(context)
    }
  }

  private def exportWithFlatgraphFormat(
    cpg: Cpg,
    repr: Representation.Value,
    outDir: Path,
    exporter: flatgraph.formats.Exporter
  ): Unit = {
    ??? // TODO wire up for graphml etc.
    val ExportResult(nodeCount, edgeCount, _, additionalInfo) = repr match {
      case Representation.All =>
//        exporter.runExport(cpg.graph, outDir)
        ???
      case Representation.Cpg =>
        ???
      case other =>
        throw new NotImplementedError(s"repr=$repr not yet supported for this format")
    }

    println(s"exported $nodeCount nodes, $edgeCount edges into $outDir")
    additionalInfo.foreach(println)
  }

}
