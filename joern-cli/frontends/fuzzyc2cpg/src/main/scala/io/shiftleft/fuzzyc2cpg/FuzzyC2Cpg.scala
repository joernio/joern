package io.shiftleft.fuzzyc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.fuzzyc2cpg.passes.{AstCreationPass, StubRemovalPass}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.x2cpg.{SourceFiles, X2Cpg, X2CpgConfig}
import org.slf4j.LoggerFactory
import scopt.OParser

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

case class Global(usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]())

class FuzzyC2Cpg() {

  def runAndOutput(sourcePaths: Set[String],
                   sourceFileExtensions: Set[String],
                   optionalOutputPath: Option[String] = None): Cpg = {
    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool = new IntervalKeyPool(100, 1000100)
    val functionKeyPool = new IntervalKeyPool(1000100, Long.MaxValue)

    val cpg = newEmptyCpg(optionalOutputPath)
    val sourceFileNames = SourceFiles.determine(sourcePaths, sourceFileExtensions)

    new MetaDataPass(cpg, Languages.C, Some(metaDataKeyPool)).createAndApply()
    val astCreator = new AstCreationPass(sourceFileNames, cpg, functionKeyPool)
    astCreator.createAndApply()
    new StubRemovalPass(cpg).createAndApply()
    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg, Some(typesKeyPool)).createAndApply()
    cpg
  }

}

object FuzzyC2Cpg {

  private val logger = LoggerFactory.getLogger(classOf[FuzzyC2Cpg])

  final case class Config(inputPaths: Set[String] = Set.empty,
                          outputPath: String = X2CpgConfig.defaultOutputPath,
                          sourceFileExtensions: Set[String] = Set(".c", ".cc", ".cpp", ".h", ".hpp"))
      extends X2CpgConfig[Config] {

    override def withAdditionalInputPath(inputPath: String): Config = copy(inputPaths = inputPaths + inputPath)
    override def withOutputPath(x: String): Config = copy(outputPath = x)
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
            "source file extensions to include when gathering source files. Defaults are .c, .cc, .cpp, .h and .hpp")
          .action((pat, cfg) => cfg.copy(sourceFileExtensions = cfg.sourceFileExtensions + pat)),
      )
    }

    X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
      case Some(config) =>
        try {
          val fuzzyc = new FuzzyC2Cpg()
          val cpg = fuzzyc.runAndOutput(config.inputPaths, config.sourceFileExtensions, Some(config.outputPath))
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
