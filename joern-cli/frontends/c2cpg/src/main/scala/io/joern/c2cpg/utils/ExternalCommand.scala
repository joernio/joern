package io.joern.c2cpg.utils

import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}

object ExternalCommand {

  private val shellPrefix: List[String] =
    if (scala.util.Properties.isWin) {
      "cmd" :: "/c" :: Nil
    } else {
      "sh" :: "-c" :: Nil
    }

  def run(command: String): Try[Seq[String]] = {
    val result = mutable.ArrayBuffer.empty[String]
    val lineHandler: String => Unit = result.addOne

    Process(shellPrefix :+ command).!(ProcessLogger(lineHandler, lineHandler)) match {
      case 0 =>
        Success(result.toSeq)
      case _ =>
        Failure(new RuntimeException(result.mkString(System.lineSeparator())))
    }
  }
}
