package io.joern.x2cpg.utils.server

import java.nio.file.Path
import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success}
import org.slf4j.LoggerFactory

class HttpExecutor(locator: ExecutableLocator, defaultArgs: Seq[String] = Nil, prepareInput: Path => Path = identity)
    extends FrontendExecutor {

  private val logger = LoggerFactory.getLogger(getClass)

  private var serverProcess: Option[Process] = None
  private val marker                         = "FrontendHTTPServer started on port"
  private val port: CompletableFuture[Int]   = new CompletableFuture[Int]()

  override def isAvailable: Boolean = {
    locator.isAvailable()
  }

  private def stdOutHandler: String => Unit = { output =>
    logger.info(output)
    if (!port.isDone && output.startsWith(marker)) {
      port.complete(Integer.parseInt(output.substring(marker.length + 1)))
    }
  }

  private def stdErrHandler: String => Unit = { output =>
    logger.warn(output)
  }

  private def start(): Unit = this.synchronized {
    if (serverProcess.isEmpty) {
      val process =
        Process(List(locator.resolve().toString, ".", "--server", "--server-timeout-minutes=1"))
          .run(ProcessLogger(stdOutHandler, stdErrHandler))

      while (process.isAlive && !port.isDone) {
        try {
          port.get(100, TimeUnit.MILLISECONDS)
        } catch {
          case _: TimeoutException =>
        }
      }
      serverProcess = Some(process)
    }
  }

  override def shutdown(): Unit = this.synchronized {
    serverProcess.foreach(_.destroy())
    serverProcess = None
  }

  private def parseArguments(args: Seq[String]): Seq[(String, Option[String])] = {
    args match {
      case Seq() => Seq.empty
      case argument +: value +: rest if argument.startsWith("--") && !value.startsWith("-") =>
        (argument.stripPrefix("--") -> Some(value)) +: parseArguments(rest)
      case argument +: rest =>
        (argument.stripPrefix("--") -> None) +: parseArguments(rest)
    }
  }

  override def execute(sourceCodeDir: Path, cpgOutputPath: Path, extraArgs: String*): Unit = {
    start()

    val arguments =
      Seq(
        "input"                        -> Some(prepareInput(sourceCodeDir).toString),
        "output"                       -> Some(cpgOutputPath.toString),
        "enable-early-schema-checking" -> None
      ) ++ parseArguments(defaultArgs ++ extraArgs)

    val client  = FrontendHTTPClient(port.getNow(-1))
    val request = client.buildRequest(arguments*)
    client.sendRequest(request) match {
      case Failure(exception) => throw new RuntimeException(exception.getMessage, exception)
      case Success(_)         => ()
    }
  }
}
