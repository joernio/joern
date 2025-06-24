package io.shiftleft.semanticcpg.utils

import scala.util.{Failure, Success, Try}
import ExternalCommand.logger

case class ExternalCommandResult(
  exitCode: Int,
  stdOut: Seq[String],
  stdErr: Seq[String],
  input: String,
  additionalContext: Option[String]
) {

  /** Lines of standard output (if successful), or else a combination of stdout and stderr, plus some context.
    */
  def stdOutAndError: Seq[String] =
    stdOut ++ stdErr

  def logIfFailed(): this.type = {
    if (exitCode != 0) {
      logger.error(s"""Process exited with code $exitCode.
           |${additionalContext.getOrElse("")}
           |Input: $input
           |Output: $stdOutAndError
           |""".stripMargin)
    }
    this
  }

  /** convenience method: verify that the result is a success, throws an exception otherwise */
  def verifySuccess(): this.type = {
    toTry.get
    this
  }

  /** Lines of standard output, if successful. */
  def successOption: Option[Seq[String]] =
    toTry.toOption

  def toTry: Try[Seq[String]] = {
    exitCode match {
      case 0 =>
        Success(stdOut)
      case nonZeroExitCode =>
        Failure(new RuntimeException(stdErr.mkString("\n")))
    }
  }
}
