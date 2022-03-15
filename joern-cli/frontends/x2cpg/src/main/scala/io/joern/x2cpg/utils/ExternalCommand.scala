package io.joern.x2cpg.utils

import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}

object ExternalCommand {

  private val IS_WIN: Boolean =
    scala.util.Properties.isWin

  private val shellPrefix: Seq[String] =
    if (IS_WIN) "cmd" :: "/c" :: Nil else "sh" :: "-c" :: Nil

  def run(command: String, cwd: String): Try[Seq[String]] = {
    val result                      = mutable.ArrayBuffer.empty[String]
    val lineHandler: String => Unit = result.addOne
    Process(shellPrefix :+ command, new java.io.File(cwd)).!(ProcessLogger(lineHandler, lineHandler)) match {
      case 0 =>
        Success(result.toSeq)
      case _ =>
        Failure(new RuntimeException(result.mkString(System.lineSeparator())))
    }
  }

}
