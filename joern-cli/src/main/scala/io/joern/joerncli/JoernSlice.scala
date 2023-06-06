package io.joern.joerncli

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.joerncli.JoernParse.ParserConfig
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.layers.Base
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.language.postfixOps
import scala.util.Using

object JoernSlice {

  import io.joern.dataflowengineoss.slicing._

  private val configParser = new scopt.OptionParser[BaseConfig]("joern-slice") {
    head("Extract intra-procedural slices from the CPG.")
    help("help")
    arg[String]("cpg")
      .text("input CPG file name - defaults to `cpg.bin`")
      .optional()
      .action { (x, c) =>
        val path = File(x)
        if (!path.isRegularFile) failure(s"File at '$x' not found or not regular, e.g. a directory.")
        c match {
          case x: SliceConfig    => x.copy(inputPath = path)
          case x: DataFlowConfig => x.copy(inputPath = path)
          case x: UsagesConfig   => x.copy(inputPath = path)
          case _                 => SliceConfig(inputPath = path)
        }
      }
    opt[String]('o', "out")
      .text("the output file to write slices to - defaults to `slices`. The file is suffixed based on the mode.")
      .action((x, c) =>
        c match {
          case c: SliceConfig    => c.copy(outFile = File(x))
          case c: DataFlowConfig => c.copy(outFile = File(x))
          case c: UsagesConfig   => c.copy(outFile = File(x))
          case _                 => SliceConfig(outFile = File(x))
        }
      )
    opt[Unit]("dummy-types")
      .text(s"for generating CPGs that use type recovery, enables the use of dummy types - defaults to false.")
      .action((_, c) =>
        c match {
          case c: SliceConfig    => c.copy(dummyTypesEnabled = true)
          case c: DataFlowConfig => c.copy(dummyTypesEnabled = true)
          case c: UsagesConfig   => c.copy(dummyTypesEnabled = true)
          case _                 => SliceConfig(dummyTypesEnabled = true)
        }
      )
    opt[String]("file-filter")
      .text(s"the name of the source file to generate slices from.")
      .action((x, c) =>
        c match {
          case c: SliceConfig    => c.copy(fileFilter = Option(x))
          case c: DataFlowConfig => c.copy(fileFilter = Option(x))
          case c: UsagesConfig   => c.copy(fileFilter = Option(x))
          case _                 => SliceConfig(fileFilter = Option(x))
        }
      )
    cmd("data-flow")
      .action((_, c) => DataFlowConfig(c.inputPath, c.outFile, c.dummyTypesEnabled))
      .children(
        opt[Int]("slice-depth")
          .text(s"the max depth to traverse the DDG for the data-flow slice - defaults to 20.")
          .action((x, c) =>
            c match {
              case c: DataFlowConfig => c.copy(sliceDepth = x)
              case _                 => c
            }
          )
      )
    cmd("usages")
      .action((_, c) => UsagesConfig(c.inputPath, c.outFile, c.dummyTypesEnabled))
      .children(
        opt[Int]("min-num-calls")
          .text(s"the minimum number of calls required for a usage slice - defaults to 1.")
          .action((x, c) =>
            c match {
              case c: UsagesConfig => c.copy(minNumCalls = x)
              case _               => c
            }
          ),
        opt[Unit]("exclude-operators")
          .text(s"excludes operator calls in the slices - defaults to false.")
          .action((_, c) =>
            c match {
              case c: UsagesConfig => c.copy(excludeOperatorCalls = true)
              case _               => c
            }
          )
      )
  }

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      if (config.isInstanceOf[SliceConfig]) {
        configParser.reportError("No command specified! Use --help for more information.")
      } else {
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
          (config match {
            case x: DataFlowConfig => Option(DataFlowSlicing.calculateDataFlowSlice(cpg, x))
            case x: UsagesConfig   => Option(UsageSlicing.calculateUsageSlice(cpg, x))
            case _                 => None
          }) match {
            case Some(programSlice: ProgramSlice) => saveSlice(config.outFile, programSlice)
            case None                             =>
          }
        }
      }
    }
  }

  /** Makes sure necessary passes are overlaid
    */
  private def checkAndApplyOverlays(cpg: Cpg): Unit = {
    import io.shiftleft.semanticcpg.language._

    if (!cpg.metaData.overlays.contains(Base.overlayName)) {
      println("Default overlays are not detected, applying defaults now")
      X2Cpg.applyDefaultOverlays(cpg)
    }
    if (!cpg.metaData.overlays.contains(OssDataFlow.overlayName)) {
      println("Data-flow overlay is not detected, applying now")
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
    }
  }

  private def generateTempCpg(config: BaseConfig): String = {
    val tmpFile = File.newTemporaryFile("joern-slice", ".bin")
    println(s"Generating CPG from code at ${config.inputPath.pathAsString}")
    (JoernParse.run(
      ParserConfig(config.inputPath.pathAsString, outputCpgFile = tmpFile.pathAsString),
      if (config.dummyTypesEnabled) List.empty else List("--no-dummyTypes")
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

  private def parseConfig(args: Array[String]): Option[BaseConfig] =
    configParser.parse(args, SliceConfig())

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
