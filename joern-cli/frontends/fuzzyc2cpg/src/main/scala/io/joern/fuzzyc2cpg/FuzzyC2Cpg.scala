package io.joern.fuzzyc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.fuzzyc2cpg.passes.{AstCreationPass, StubRemovalPass}
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.{SourceFiles, X2Cpg, X2CpgConfig}
import org.slf4j.LoggerFactory
import scopt.OParser

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

case class Global(usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]())

class FuzzyC2Cpg() {

  def runAndOutput(
    sourcePaths: Set[String],
    sourceFileExtensions: Set[String],
    optionalOutputPath: Option[String] = None
  ): Cpg = {

    val cpg             = newEmptyCpg(optionalOutputPath)
    val sourceFileNames = SourceFiles.determine(sourcePaths, sourceFileExtensions)

    new MetaDataPass(cpg, Languages.C).createAndApply()
    val astCreator = new AstCreationPass(sourceFileNames, cpg)
    astCreator.createAndApply()
    new StubRemovalPass(cpg).createAndApply()
    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg).createAndApply()
    cpg
  }

}

object FuzzyC2Cpg {

  private val logger = LoggerFactory.getLogger(classOf[FuzzyC2Cpg])

  final case class Config(
    inputPaths: Set[String] = Set.empty,
    outputPath: String = X2CpgConfig.defaultOutputPath,
    sourceFileExtensions: Set[String] = Set(".c", ".cc", ".cpp", ".h", ".hpp")
  ) extends X2CpgConfig[Config] {

    override def withInputPath(inputPath: String): Config = copy(inputPaths = inputPaths + inputPath)
    override def withOutputPath(x: String): Config        = copy(outputPath = x)
  }

  def main(args: Array[String]): Unit = {

    val frontendSpecificOptions = {
      val builder = OParser.builder[Config]
      import builder._
      OParser.sequence(
        programName(classOf[FuzzyC2Cpg].getSimpleName),
        opt[String]("out")
          .text("(DEPRECATED use `output`) output filename")
          .action { (x, c) =>
            logger.warn("`--out` is DEPRECATED. Use `--output` instead")
            c.withOutputPath(x)
          },
        opt[String]("source-file-ext")
          .unbounded()
          .text(
            "source file extensions to include when gathering source files. Defaults are .c, .cc, .cpp, .h and .hpp"
          )
          .action((pat, cfg) => cfg.copy(sourceFileExtensions = cfg.sourceFileExtensions + pat))
      )
    }

    X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
      case Some(config) =>
        try {
          val fuzzyc = new FuzzyC2Cpg()
          val cpg    = fuzzyc.runAndOutput(config.inputPaths, config.sourceFileExtensions, Some(config.outputPath))
          cpg.close()
        } catch {
          case NonFatal(ex) =>
            logger.error("Failed to generate CPG.", ex)
            System.exit(1)
        }
      case None =>
        System.exit(1)
    }
  }

}
