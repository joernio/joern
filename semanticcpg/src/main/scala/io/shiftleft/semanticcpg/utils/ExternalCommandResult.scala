package io.shiftleft.semanticcpg.utils

import io.shiftleft.semanticcpg.utils.ExternalCommand.logger

import scala.util.{Failure, Success, Try}

case class ExternalCommandResult(
  exitCode: Int,
  stdOut: Seq[String],
  stdErr: Seq[String],
  input: String,
  additionalContext: Option[String]
) {

  def successful: Boolean =
    exitCode == 0

  /** @return
    *   Lines of standard output (if successful), or else a combination of stdout and stderr.
    */
  def stdOutAndError: Seq[String] =
    stdOut ++ stdErr

  /** Logs details about the command execution if it failed (non-zero exit code).
    *
    * @param asError
    *   If true, logs as error level; otherwise logs as warning level
    * @return
    *   This instance (for method chaining)
    */
  def logIfFailed(asError: Boolean = true): this.type = {
    if (exitCode != 0) {
      val msg = s"""Process exited with code $exitCode.
       |Context: ${additionalContext.getOrElse("None provided.")}
       |Input: $input
       |Output: ${stdOutAndError.mkString("\n")}
       |""".stripMargin
      if (asError) logger.error(msg) else logger.warn(msg)
    }
    this
  }

  /** Convenience method: verify that the result is a success, throws an exception otherwise
    * @return
    *   This instance (for method chaining)
    */
  def verifySuccess(): this.type = {
    toTry.get
    this
  }

  /** @return
    *   Lines of standard output, if successful.
    */
  def successOption: Option[Seq[String]] =
    toTry.toOption

  /** @return
    *   Lines of stdout, if successful. Otherwise, an exception with message=stderr.
    */
  def toTry: Try[Seq[String]] = {
    if (successful) Success(stdOut)
    else Failure(new RuntimeException(stdErr.mkString("\n")))
  }
}
