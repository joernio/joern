package io.joern.jssrc2cpg.utils

import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}

object Environment {

  private var isValid: Option[Boolean] = None

  private val logger = LoggerFactory.getLogger(getClass)

  def valid(): Boolean = isValid match {
    case Some(value) =>
      value
    case None =>
      isValid = Some(nodeAvailable())
      isValid.get
  }

  private def nodeAvailable(): Boolean = {
    logger.debug(s"\t+ Checking node ...")
    ExternalCommand.run("node -v", ".") match {
      case Success(result) =>
        logger.debug(s"\t+ node is available (version: ${result.headOption.getOrElse("unknown")})")
        true
      case Failure(_) =>
        logger.error("\t- node is not installed.")
        false
    }
  }

}
