package io.joern.joerncli

import better.files.File
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import io.joern.joerncli.JoernParse.ParserConfig
import io.joern.joerncli.slicing._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters.MapHasAsScala
import scala.language.postfixOps
import scala.util.Using

/** The kind of mode to use for slicing.
  */
object SliceMode extends Enumeration {
  type SliceModes = Value
  val DataFlow, Usages = Value
}

object JoernSlice {

  import io.joern.joerncli.SliceMode._

  implicit val sliceModeRead: scopt.Read[SliceModes] =
    scopt.Read.reads(SliceMode withName)

  case class Config(
    inputPath: File = File("cpg.bin"),
    outFile: File = File("slices"),
    sliceMode: SliceModes = DataFlow,
    sourceFile: Option[String] = None,
    sliceDepth: Int = 20,
    minNumCalls: Int = 1
  )

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      val inputCpgPath =
        if (config.inputPath.isDirectory) generateTempCpg(config)
        else config.inputPath.pathAsString
      Using.resource(CpgBasedTool.loadFromOdb(inputCpgPath)) { cpg =>
        val slice: ProgramSlice = config.sliceMode match {
          case DataFlow => DataFlowSlicing.calculateDataFlowSlice(cpg, config)
          case Usages   => UsageSlicing.calculateUsageSlice(cpg, config)
        }
        storeSliceInNewCpg(config.outFile, slice)
      }
    }
  }

  private def generateTempCpg(config: Config): String = {
    val tmpFile = File.newTemporaryFile("joern-slice", ".bin")
    println(s"Generating CPG from code at ${config.inputPath.pathAsString}")
    (JoernParse.run(ParserConfig(config.inputPath.pathAsString, outputCpgFile = tmpFile.pathAsString)) match {
      case Right(_) =>
        println(s"Temporary CPG has been successfully generated at ${tmpFile.pathAsString}")
        Right(tmpFile.deleteOnExit(swallowIOExceptions = true).pathAsString)
      case x => x
    }) match {
      case Left(err)   => throw new RuntimeException(err)
      case Right(path) => path
    }
  }

  private def parseConfig(args: Array[String]): Option[Config] =
    new scopt.OptionParser[Config]("joern-slice") {
      head("Extract intra-procedural slices from the CPG.")
      help("help")
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action { (x, c) =>
          val path = File(x)
          if (!path.isRegularFile) failure(s"File at '$x' not found or not regular, e.g. a directory.")
          c.copy(inputPath = path)
        }
      opt[String]('o', "out")
        .text("the output file to write slices to - defaults to `slices`. The file is suffixed based on the mode.")
        .action((x, c) => c.copy(outFile = File(x)))
      opt[SliceModes]('m', "mode")
        .text(s"the kind of slicing to perform - defaults to `DataFlow`. Options: [${SliceMode.values.mkString(", ")}]")
        .action((x, c) => c.copy(sliceMode = x))
      opt[String]("source-file")
        .text("the name of the source file to generate slices from.")
        .optional()
        .action((x, c) => c.copy(sourceFile = Some(x)))
      opt[Int]("slice-depth")
        .text(s"the max depth to traverse the DDG for the data-flow slice (for `DataFlow` mode) - defaults to 20.")
        .action((x, c) => c.copy(minNumCalls = x))
      opt[Int]("min-num-calls")
        .text(s"the minimum number of calls required for a usage slice (for `Usage` mode) - defaults to 1.")
        .action((x, c) => c.copy(minNumCalls = x))

    }.parse(args, Config())

  private def storeSliceInNewCpg(outFile: File, programSlice: ProgramSlice): Unit = {

    def storeDataFlowSlices(cpg: Cpg, slices: Set[DataFlowSlice]): Unit = {
      val graph = cpg.graph
      slices.foreach { slice =>
        slice.nodes.foreach { node =>
          val keyValueSequence = node.propertiesMap().asScala.toList.flatMap { case (k, v) => List[Any](k, v) }
          if (Option(graph.node(node.id())).isEmpty) graph.addNode(node.id(), node.label, keyValueSequence: _*)
        }
        slice.nodes.foreach { node =>
          val outNode = graph.node(node.id())
          slice.edges.get(node).toList.foreach { edges =>
            edges.foreach { edge =>
              val inNode = graph.node(edge.inNode().id())
              if (!outNode.out(edge.label()).exists(_.id().equals(inNode.id())))
                outNode.addEdge(edge.label, inNode)
            }
          }
        }
      }
    }

    def normalizePath(path: String, ext: String): String =
      if (path.endsWith(ext)) path
      else path + ext

    val finalOutputPath = programSlice match {
      case ProgramDataFlowSlice(dataFlowSlices) =>
        val sliceCpg = File(normalizePath(outFile.pathAsString, ".cpg")).createFileIfNotExists()
        Using.resource(Cpg.withStorage(sliceCpg.pathAsString)) { newCpg =>
          storeDataFlowSlices(newCpg, dataFlowSlices.flatMap(_._2).toSet)
        }
        sliceCpg.pathAsString
      case programUsageSlice: ProgramUsageSlice =>
        val sliceCpg = File(normalizePath(outFile.pathAsString, ".json")).createFileIfNotExists()
        sliceCpg.write(programUsageSlice.asJson.spaces2)
        sliceCpg.pathAsString
    }
    println(s"Slices have been successfully generated and written to $finalOutputPath")
  }

}
