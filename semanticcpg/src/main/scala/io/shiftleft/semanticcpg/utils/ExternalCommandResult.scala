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

  def successful: Boolean =
    exitCode == 0

  /** Lines of standard output (if successful), or else a combination of stdout and stderr, plus some context.
    */
  def stdOutAndError: Seq[String] =
    stdOut ++ stdErr

  def logIfFailed(): this.type = {
    if (exitCode != 0) {
      logger.error(s"""Process exited with code $exitCode.
           |${additionalContext.getOrElse("")}
           |Input: $input
           |Output: ${stdOutAndError.mkString("\n")}
           |""".stripMargin)
    }
    this
  }

  /** Same as [[logIfFailed]] for non-zero exits, but also logs the command's output at debug level when it succeeded.
    * Useful for tools that exit 0 even when they did nothing useful (e.g. delombok writing files back unchanged).
    */
  def logAlways(): this.type = {
    if (exitCode != 0) {
      logIfFailed()
    } else {
      logger.debug(s"""Process exited with code 0.
           |${additionalContext.getOrElse("")}
           |Input: $input
           |Output: ${stdOutAndError.mkString("\n")}
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

  /** Lines of stdout, if successful. Otherwise an exception with message=stderr. */
  def toTry: Try[Seq[String]] = {
    if (successful) Success(stdOut)
    else Failure(new RuntimeException(stdErr.mkString("\n")))
  }
}
