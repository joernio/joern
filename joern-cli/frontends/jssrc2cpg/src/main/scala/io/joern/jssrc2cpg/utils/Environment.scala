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
        logger.error("\t- astgen is not installed.")
        false
    }
  }

}
