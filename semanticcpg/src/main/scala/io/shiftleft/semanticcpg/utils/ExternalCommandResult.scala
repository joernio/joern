package io.shiftleft.semanticcpg.utils

import scala.util.{Failure, Success, Try}
import ExternalCommand.logger

case class ExternalCommandResult(exitCode: Int, stdOut: Seq[String], stdErr: Seq[String], input: String, additionalContext: Option[String]) {
  
  /** Lines of standard output (if successful),
    * or else a combination of stdout and stderr, plus some context. */
  def getOutputText: String = {
    exitCode match {
      case 0 => stdOut.mkString("\n")
      case nonZeroExitCode =>
        val allOutput = stdOut ++ stdErr
        s"""Process exited with code $nonZeroExitCode.
           |${additionalContext.getOrElse("")}
           |Input: $input
           |Output:
           |${allOutput.mkString("\n")}
           |""".stripMargin
    }
  }

  def logIfFailed(): this.type = {
    if (exitCode != 0) {
      logger.error(getOutputText)
    }
    this
  }

  /** convenience method: verify that the result is a success, throws an exception otherwise */
  def verifySuccess(): this.type = {
    toTry.get
    this
  }

  /** Lines of standard output, if successful. */
  def successOption: Option[String] =
    toTry.toOption

  def toTry: Try[String] = {
    exitCode match {
      case 0 => Success(getOutputText)
      case nonZeroExitCode => Failure(new RuntimeException(getOutputText))
    }
  }
}
