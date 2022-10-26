package io.joern.joerncli

import better.files.Dsl._
import better.files.File
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.joern.x2cpg.layers._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{toAstNodeMethods, toNodeTypeStarters}
import io.shiftleft.semanticcpg.layers._
import overflowdb.formats.ExportResult
import overflowdb.formats.dot.DotExporter
import overflowdb.formats.graphml.GraphMLExporter
import overflowdb.formats.graphson.GraphSONExporter
import overflowdb.formats.neo4jcsv.Neo4jCsvExporter
import overflowdb.{Edge, Node}

import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Using

object JoernExport extends App {

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
          s"representation to extract: [${Representation.values.toSeq.map(_.toString.toLowerCase).sorted.mkString("|")}] - defaults to `${Representation.Cpg14}`"
        )
        .action((x, c) => c.copy(repr = Representation.withNameIgnoreCase(x)))
      opt[String]("format")
        .action((x, c) => c.copy(format = Format.withNameIgnoreCase(x)))
        .text(
          s"export format, one of [${Format.values.toSeq.map(_.toString.toLowerCase).sorted.mkString("|")}] - defaults to `${Format.Dot}`"
        )
    }.parse(args, Config())

  parseConfig.foreach { config =>
    val repr   = config.repr
    val outDir = config.outDir
    exitIfInvalid(outDir, config.cpgFileName)
    mkdir(File(outDir))

    Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
      exportCpg(cpg, config.repr, config.format, Paths.get(outDir).toAbsolutePath)
    }
  }

  def exportCpg(cpg: Cpg, representation: Representation.Value, format: Format.Value, outDir: Path): Unit = {
    implicit val semantics: Semantics = DefaultSemantics()
    if (semantics.elements.isEmpty) {
      System.err.println("Warning: semantics are empty.")
    }

    CpgBasedTool.addDataFlowOverlayIfNonExistent(cpg)
    val context = new LayerCreatorContext(cpg)

    format match {
      case Format.Dot =>
        exportDot(representation, outDir, context)
      case Format.Neo4jCsv =>
        exportWithOdbFormat(cpg, representation, outDir, Neo4jCsvExporter)
      case Format.Graphml =>
        exportWithOdbFormat(cpg, representation, outDir, GraphMLExporter)
      case Format.Dot =>
        exportWithOdbFormat(cpg, representation, outDir, DotExporter)
      case Format.Graphson =>
        exportWithOdbFormat(cpg, representation, outDir, GraphSONExporter)
    }
  }

  private def exportDot(repr: Representation.Value, outDir: Path, context: LayerCreatorContext): Unit = {
    val outDirStr = outDir.toString
    import Representation._
    repr match {
      case Ast   => new DumpAst(AstDumpOptions(outDirStr)).create(context)
      case Cfg   => new DumpCfg(CfgDumpOptions(outDirStr)).create(context)
      case Ddg   => new DumpDdg(DdgDumpOptions(outDirStr)).create(context)
      case Cdg   => new DumpCdg(CdgDumpOptions(outDirStr)).create(context)
      case Pdg   => new DumpPdg(PdgDumpOptions(outDirStr)).create(context)
      case Cpg14 => new DumpCpg14(Cpg14DumpOptions(outDirStr)).create(context)
    }
  }

  private def exportWithOdbFormat(
    cpg: Cpg,
    repr: Representation.Value,
    outDir: Path,
    exporter: overflowdb.formats.Exporter
  ): Unit = {
    val ExportResult(nodeCount, edgeCount, _, additionalInfo) = repr match {
      case Representation.All =>
        exporter.runExport(cpg.graph, outDir)
      case Representation.Cpg =>
        splitByMethod(cpg).iterator
          .map { subGraph =>
            val name = subGraph.name
            val sanitizedFileName =
              if (name.startsWith("/")) s"_root_/$name"
              else name
            val extension   = exporter.defaultFileExtension
            val outFileName = outDir.resolve(s"$sanitizedFileName.$extension")
            exporter.runExport(subGraph.nodes, subGraph.edges, outFileName)
          }
          .reduce(plus)
    }

    println(s"exported $nodeCount nodes, $edgeCount edges into $outDir")
    additionalInfo.foreach(println)
  }

  /** for each method in the cpg: recursively traverse all AST edges to get the subgraph of nodes within this method add
    * the method and this subgraph to the export add all edges between all of these nodes to the export
    */
  private def splitByMethod(cpg: Cpg): IterableOnce[SubGraph] = {
    cpg.method.map { method =>
      val sanitizedMethodName = method.name.replaceAll("[^a-zA-Z0-9-_\\.]", "_")
      SubGraph(name = s"${method.filename}/$sanitizedMethodName", nodes = method.ast.toSet)
    }
  }

  private def plus(resultA: ExportResult, resultB: ExportResult): ExportResult = {
    ExportResult(
      nodeCount = resultA.nodeCount + resultB.nodeCount,
      edgeCount = resultA.edgeCount + resultB.edgeCount,
      files = resultA.files ++ resultB.files,
      additionalInfo = resultA.additionalInfo
    )
  }

  case class SubGraph(name: String, nodes: Set[Node]) {
    def edges: Set[Edge] = {
      for {
        node <- nodes
        edge <- node.bothE.asScala
        if nodes.contains(edge.inNode) && nodes.contains(edge.outNode)
      } yield edge
    }
  }
}
