package io.joern.jssrc2cpg.utils

import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success}

object Environment {

  val IS_WIN: Boolean   = scala.util.Properties.isWin
  val IS_MAC: Boolean   = scala.util.Properties.isMac
  val IS_LINUX: Boolean = scala.util.Properties.isLinux

  private var isValid: Option[Boolean] = None

  private val logger = LoggerFactory.getLogger(getClass)

  def pathExists(path: String): Boolean = {
    if (!Paths.get(path).toFile.exists()) {
      logger.error(s"Input path '$path' does not exist!")
      false
    } else {
      true
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
