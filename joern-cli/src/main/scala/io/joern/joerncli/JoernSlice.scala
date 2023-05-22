package io.joern.joerncli

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.joerncli.JoernParse.ParserConfig
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.language.postfixOps
import scala.util.Using

object JoernSlice {

  import io.joern.slicing.SliceMode._
  import io.joern.slicing._

  implicit val sliceModeRead: scopt.Read[SliceModes] =
    scopt.Read.reads(SliceMode withName)

  case class Config(
    inputPath: File = File("cpg.bin"),
    outFile: File = File("slices"),
    sliceMode: SliceModes = DataFlow,
    sourceFile: Option[String] = None,
    sliceDepth: Int = 20,
    minNumCalls: Int = 1,
    typeRecoveryDummyTypes: Boolean = false,
    excludeOperatorCalls: Boolean = false
  )

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      val inputCpgPath =
        if (
          config.inputPath.isDirectory || !config.inputPath
            .extension(includeDot = false)
            .exists(_.matches("(bin|cpg)"))
        )
          generateTempCpg(config)
        else config.inputPath.pathAsString
      Using.resource(CpgBasedTool.loadFromOdb(inputCpgPath)) { cpg =>
        checkAndApplyOverlays(cpg)
        // Slice the CPG
        val slice: ProgramSlice = config.sliceMode match {
          case DataFlow => DataFlowSlicing.calculateDataFlowSlice(cpg, config)
          case Usages   => UsageSlicing.calculateUsageSlice(cpg, config)
        }
        saveSlice(config.outFile, slice)
      }
    }
  }

  /** Makes sure necessary passes are overlaid
    */
  private def checkAndApplyOverlays(cpg: Cpg): Unit = {
    import io.shiftleft.semanticcpg.language._

    if (!cpg.metaData.overlays.contains("base")) {
      println("Default overlays are not detected, applying defaults now")
      X2Cpg.applyDefaultOverlays(cpg)
    }
    if (!cpg.metaData.overlays.contains("dataflowOss")) {
      println("Data-flow overlay is not detected, applying now")
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
    }
  }

  private def generateTempCpg(config: SliceConfig): String = {
    val tmpFile = File.newTemporaryFile("joern-slice", ".bin")
    println(s"Generating CPG from code at ${config.inputPath.pathAsString}")
    (JoernParse.run(
      ParserConfig(config.inputPath.pathAsString, outputCpgFile = tmpFile.pathAsString),
      if (config.typeRecoveryDummyTypes) List.empty else List("--no-dummyTypes")
    ) match {
      case Right(_) =>
        println(s"Temporary CPG has been successfully generated at ${tmpFile.pathAsString}")
        Right(tmpFile.deleteOnExit(swallowIOExceptions = true).pathAsString)
      case x => x
    }) match {
      case Left(err)   => throw new RuntimeException(err)
      case Right(path) => path
    }
  }

  private def parseConfig(args: Array[String]): Option[SliceConfig] =
    new scopt.OptionParser[SliceConfig]("joern-slice") {
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
      opt[Boolean]("dummy-types")
        .text(s"for generating CPGs that use type recovery, enables the use of dummy types - defaults to false.")
        .action((x, c) => c.copy(typeRecoveryDummyTypes = x))
      opt[Boolean]("exclude-operators")
        .text(s"excludes operator calls in the slices - defaults to false.")
        .action((x, c) => c.copy(excludeOperatorCalls = x))

    }.parse(args, SliceConfig())

  private def saveSlice(outFile: File, programSlice: ProgramSlice): Unit = {

    def normalizePath(path: String, ext: String): String =
      if (path.endsWith(ext)) path
      else path + ext

    val finalOutputPath =
      File(normalizePath(outFile.pathAsString, ".json"))
        .createFileIfNotExists()
        .write(programSlice.toJsonPretty)
        .pathAsString
    println(s"Slices have been successfully generated and written to $finalOutputPath")
  }

}
