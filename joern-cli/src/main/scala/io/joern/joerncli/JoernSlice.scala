package io.joern.joerncli

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.joerncli.JoernParse.ParserConfig
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.layers.Base
import io.shiftleft.semanticcpg.utils.FileUtil.*

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Files, Path, Paths}
import scala.language.postfixOps
import scala.util.{Try, Using}

object JoernSlice {

  import io.joern.dataflowengineoss.slicing.*

  private val configParser = new scopt.OptionParser[BaseConfig[?]]("joern-slice") {
    head("Extract various slices from the CPG.")
    help("help")
    arg[String]("cpg")
      .text("input CPG file name, or source code - defaults to `cpg.bin`")
      .optional()
      .action((x, c) => c.withInputPath(Paths.get(x)))
      .validate { x =>
        val path = Paths.get(x)
        if (Files.isRegularFile(path) || Files.isDirectory(path)) success
        else failure(s"File at '$x' not found or not regular, e.g. a directory or source file.")
      }
    opt[String]('o', "out")
      .text("the output file to write slices to - defaults to `slices`. The file is suffixed based on the mode.")
      .action((x, c) => c.withOutputSliceFile(Paths.get(x)))
    opt[Unit]("dummy-types")
      .text(s"for generating CPGs that use type recovery, enables the use of dummy types - defaults to false.")
      .action((_, c) => c.withDummyTypesEnabled(true))
    opt[String]("file-filter")
      .text(s"the name of the source file to generate slices from.")
      .action((x, c) => c.withFileFilter(Option(x)))
    opt[String]("method-name-filter")
      .text(s"filters in slices that go through specific methods by names. Uses regex.")
      .action((x, c) => c.withMethodNameFilter(Option(x)))
    opt[String]("method-parameter-filter")
      .text(s"filters in slices that go through methods with specific types on the method parameters. Uses regex.")
      .action((x, c) => c.withMethodParamTypeFilter(Option(x)))
    opt[String]("method-annotation-filter")
      .text(s"filters in slices that go through methods with specific annotations on the methods. Uses regex.")
      .action((x, c) => c.withMethodAnnotationFilter(Option(x)))
    opt[Int]('p', "parallelism")
      .text(s"the number of threads the executor pool should be specified with.")
      .action((x, c) => c.withParallelism(x))
      .validate(x => if (x <= 0) failure("Parallelism should be greater than 0") else success)
    cmd("data-flow")
      .action((_, _) => DataFlowConfig())
      .children(
        opt[Int]("slice-depth")
          .text(s"the max depth to traverse the DDG for the data-flow slice - defaults to 20.")
          .action((x, c) =>
            c match {
              case c: DataFlowConfig => c.copy(sliceDepth = x)
              case _                 => c
            }
          ),
        opt[String]("sink-filter")
          .text(s"filters on the sink's `code` property. Uses regex.")
          .action((x, c) =>
            c match {
              case c: DataFlowConfig => c.copy(sinkPatternFilter = Option(x))
              case _                 => c
            }
          ),
        opt[Unit]("end-at-external-method")
          .text(s"all slices must end at a call to an external method - defaults to false.")
          .action((_, c) =>
            c match {
              case c: DataFlowConfig => c.copy(mustEndAtExternalMethod = true)
              case _                 => c
            }
          )
      )
    cmd("usages")
      .action((_, _) => UsagesConfig())
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
          ),
        opt[Unit]("exclude-source")
          .text(s"excludes method source code in the slices - defaults to false.")
          .action((_, c) =>
            c match {
              case c: UsagesConfig => c.copy(excludeMethodSource = true)
              case _               => c
            }
          )
      )
  }

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      if (config.isInstanceOf[DefaultSliceConfig]) {
        configParser.reportError("No command specified! Use --help for more information.")
      } else {
        val inputCpgPath =
          if (
            Files.isDirectory(config.inputPath) || !config.inputPath
              .extension(includeDot = false)
              .exists(_.matches("(bin|cpg)"))
          ) {
            generateTempCpg(config).get
          } else {
            config.inputPath.toString
          }
        Using.resource(CpgBasedTool.loadFromFile(inputCpgPath)) { cpg =>
          checkAndApplyOverlays(cpg)
          // Slice the CPG
          (config match {
            case x: DataFlowConfig => DataFlowSlicing.calculateDataFlowSlice(cpg, x)
            case x: UsagesConfig   => Option(UsageSlicing.calculateUsageSlice(cpg, x))
            case _                 => None
          }) match {
            case Some(programSlice: ProgramSlice) => saveSlice(config.outputSliceFile, programSlice)
            case None                             => println("Empty slice, no file generated.")
          }
        }
      }
    }
  }

  /** Makes sure necessary passes are overlaid
    */
  private def checkAndApplyOverlays(cpg: Cpg): Unit = {
    import io.shiftleft.semanticcpg.language.*

    if (!cpg.metaData.overlays.contains(Base.overlayName)) {
      println("Default overlays are not detected, applying defaults now")
      X2Cpg.applyDefaultOverlays(cpg)
    }
    if (!cpg.metaData.overlays.contains(OssDataFlow.overlayName)) {
      println("Data-flow overlay is not detected, applying now")
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
    }
  }

  private def generateTempCpg(config: BaseConfig[?]): Try[String] = {
    val tmpFile = FileUtil.newTemporaryFile("joern-slice", ".bin")
    println(s"Generating CPG from code at ${config.inputPath.toString}")

    JoernParse
      .run(
        ParserConfig(config.inputPath.toString, outputCpgFile = tmpFile.toString),
        if (config.dummyTypesEnabled) List.empty else List("--no-dummyTypes")
      )
      .map { _ =>
        println(s"Temporary CPG has been successfully generated at ${tmpFile.toString}")
        FileUtil.deleteOnExit(tmpFile, swallowIOExceptions = true)
        tmpFile.toString
      }
  }

  private def parseConfig(args: Array[String]): Option[BaseConfig[?]] =
    configParser.parse(args, DefaultSliceConfig())

  private def saveSlice(outFile: Path, programSlice: ProgramSlice): Unit = {

    def normalizePath(path: String, ext: String): String =
      if (path.endsWith(ext)) path
      else path + ext

    val finalOutputPath =
      Paths
        .get(normalizePath(outFile.toString, ".json"))
        .createWithParentsIfNotExists()

    Files.writeString(finalOutputPath, programSlice.toJsonPretty)
    finalOutputPath.toString
    println(s"Slices have been successfully generated and written to $finalOutputPath")
  }

}
