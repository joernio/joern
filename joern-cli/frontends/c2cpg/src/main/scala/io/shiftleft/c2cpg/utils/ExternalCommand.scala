package io.shiftleft.c2cpg.utils

import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}

object ExternalCommand {
  private val windowsSystemPrefix: String = "Windows"
  private val osNameProperty: String = "os.name"
  private val systemString: String = System.getProperty(osNameProperty)

  def run(command: String): Try[Seq[String]] = {
    val result = mutable.ArrayBuffer.empty[String]
    val lineHandler: String => Unit = result.addOne
    val shellPrefix =
      if (systemString != null && systemString.startsWith(windowsSystemPrefix)) {
        "cmd" :: "/c" :: Nil
      } else {
        "sh" :: "-c" :: Nil
      }

    Process(shellPrefix :+ command).!(ProcessLogger(lineHandler, lineHandler)) match {
      case 0 =>
        Success(result.toSeq)
      case _ =>
        Failure(new RuntimeException(result.mkString(System.lineSeparator())))
    }
  }
}
