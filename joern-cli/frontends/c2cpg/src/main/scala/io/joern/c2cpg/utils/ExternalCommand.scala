package io.joern.c2cpg.utils

import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

object ExternalCommand {

  private val IS_WIN: Boolean = scala.util.Properties.isWin

  def run(command: String): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val processLogger = ProcessLogger(stdOutOutput.add, stdOutOutput.add)
    Try(Process(command).!(processLogger)) match {
      case Success(0) =>
        Success(stdOutOutput.asScala.toSeq)
      case Success(1)
          if IS_WIN && command != IncludeAutoDiscovery.GCC_VERSION_COMMAND && IncludeAutoDiscovery.gccAvailable() =>
        // the command to query the system header file locations within a Windows
        // environment always returns Success(1) for whatever reason...
        Success(stdOutOutput.asScala.toSeq)
      case _ =>
        Failure(new RuntimeException(stdOutOutput.asScala.mkString(System.lineSeparator())))
    }
  }

}
