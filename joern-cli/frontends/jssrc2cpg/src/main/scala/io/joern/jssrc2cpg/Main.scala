package io.joern.jssrc2cpg

import io.joern.jssrc2cpg.utils.Environment
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2CpgConfig
import org.slf4j.LoggerFactory
import scopt.OParser

import scala.util.control.NonFatal

final case class Config(inputPaths: Set[String] = Set.empty, outputPath: String = X2CpgConfig.defaultOutputPath)
    extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config = copy(inputPaths = inputPaths + inputPath)
  override def withOutputPath(x: String): Config                  = copy(outputPath = x)
}

object Main extends App {

  private val logger = LoggerFactory.getLogger(classOf[JsSrc2Cpg])

  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("jssrc2cpg"))
  }

  private def createCpg(config: Config): Unit =
    try {
      val cpg = new JsSrc2Cpg().createCpg(config)
      cpg.close()
    } catch {
      case NonFatal(ex) =>
        logger.error("Failed to generate CPG.", ex)
        System.exit(1)
    }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) if Environment.valid() =>
      createCpg(config)
    case _ =>
      System.exit(1)
  }

}
