package io.joern.joerncli

import scala.util.Using
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._
import io.joern.dataflowengineoss.language._
import io.joern.joerncli.console.Joern.semantics
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import overflowdb.Edge

object JoernSlice {
  case class Config(
    cpgFileName: String = "cpg.bin",
    outFile: String = "slice.bin",
    sourceFile: String = "",
    sourceLine: Int = -1
  )

  case class Slice(nodes: List[CfgNode], edges: Map[CfgNode, List[Edge]])

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
        val slice = calculateSlice(cpg, config.sourceFile, config.sourceLine)
        storeSliceInNewCpg(config.outFile, slice)
      }
    }
  }

  private def parseConfig(args: Array[String]): Option[Config] =
    new scopt.OptionParser[Config]("joern-slice") {
      head("Extract intra-procedural backward slice for a line of code")
      help("help")
      arg[String]("sourcefile")
        .text("The file holding the sink statement")
        .action((x, c) => c.copy(sourceFile = x))
      arg[String]("line")
        .text("Line number of sink statement")
        .action((x, c) => c.copy(sourceLine = x.toInt))
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]('o', "out")
        .text("output CPG file")
        .action((x, c) => c.copy(outFile = x))
    }.parse(args, Config())

  private def calculateSlice(cpg: Cpg, sourceFile: String, sourceLine: Int): Slice = {
    val sinks      = cpg.file.nameExact(sourceFile).ast.lineNumber(sourceLine).isCall.argument.l
    val sliceNodes = sinks.repeat(_.ddgIn)(_.maxDepth(20).emit).dedup.l
    val sliceEdges = sliceNodes
      .flatMap(_.outE)
      .filter(x => sliceNodes.contains(x.inNode()))
      .groupBy(_.outNode().asInstanceOf[CfgNode])
    Slice(sliceNodes, sliceEdges)
  }

  private def storeSliceInNewCpg(outFile: String, slice: Slice): Unit = {
    val newCpg = Cpg.withStorage(outFile)
    val graph  = newCpg.graph
    slice.nodes.foreach { node =>
      val keyValueSequence = node.propertiesMap().asScala.toList.flatMap { case (k, v) => List[Any](k, v) }
      graph.addNode(node.id(), node.label, keyValueSequence: _*)
    }
    slice.nodes.foreach { node =>
      val outNode = graph.node(node.id())
      slice.edges.get(node).toList.foreach { edges =>
        edges.foreach { edge =>
          val inNode = graph.node(edge.inNode().id())
          outNode.addEdge(edge.label, inNode)
        }
      }
    }
    newCpg.close()
  }

}
