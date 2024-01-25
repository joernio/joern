package io.joern.swiftsrc2cpg.utils

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object ExternalCommand extends io.joern.x2cpg.utils.ExternalCommand {

  override def handleRunResult(result: Try[Int], stdOut: Seq[String], stdErr: Seq[String]): Try[Seq[String]] = {
    result match {
      case Success(0) =>
        Success(stdOut)
      case Success(_) if stdErr.isEmpty && stdOut.nonEmpty =>
        // SwiftAstGen exits with exit code != 0 on Windows.
        // To catch with we specifically handle the empty stdErr here.
        Success(stdOut)
      case _ =>
        val allOutput = stdOut ++ stdErr
        Failure(new RuntimeException(allOutput.mkString(System.lineSeparator())))
    }
  }

}
