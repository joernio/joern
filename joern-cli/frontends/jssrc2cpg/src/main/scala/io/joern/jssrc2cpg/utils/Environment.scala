package io.joern.jssrc2cpg.utils

import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success}

object Environment {

  private var isValid: Option[Boolean] = None

  private val logger = LoggerFactory.getLogger(getClass)

  def allPathsExist(paths: Set[String]): Boolean = {
    val invalidPaths = paths.collect { case p if !Paths.get(p).toFile.exists() => p }
    if (invalidPaths.isEmpty) {
      true
    } else {
      invalidPaths.foreach(p => logger.error(s"Input path '$p' does not exist!"))
      false
    }
  }

  def valid(): Boolean = isValid match {
    case Some(value) =>
      value
    case None =>
      isValid = Some(astgenAvailable())
      isValid.get
  }

  private def astgenAvailable(): Boolean = {
    logger.debug(s"\t+ Checking astgen ...")
    ExternalCommand.run("astgen --version", ".") match {
      case Success(result) =>
        logger.debug(s"\t+ astgen is available (version: ${result.headOption.getOrElse("unknown")})")
        true
      case Failure(_) =>
        val hint =
          s"""\t- astgen is not installed! Please make sure you have 'npm' installed.
              |  Then run the astgen installation:
              |   - Linux/MacOS: 'sudo npm install -g @joernio/astgen'
              |   - Windows 'npm install -g @joernio/astgen' in a console with admin rights
              |  """.stripMargin
        logger.error(hint)
        false
    }
  }

}
