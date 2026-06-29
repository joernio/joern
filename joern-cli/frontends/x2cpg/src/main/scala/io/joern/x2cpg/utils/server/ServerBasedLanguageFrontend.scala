package io.joern.x2cpg.utils.server

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success}

trait ServerBasedLanguageFrontend {
  protected val envVar: String
  protected val fallbackPath: String
  protected val executablePath: String
  protected def cpgFileSuffix: String

  protected def executablePathOverride: Option[Path] = None

  private var serverProcess: Option[Process] = None
  private val marker                         = "FrontendHTTPServer started on port"
  private val port: CompletableFuture[Int]   = new CompletableFuture[Int]()

  private def basePath: String =
    scala.sys.env.getOrElse(envVar, fallbackPath)

  final protected def fullExecutablePath: Path =
    executablePathOverride.getOrElse(Path.of(basePath, executablePath).toRealPath())

  final def isAvailable: Boolean = Files.exists(fullExecutablePath)

  final protected def newTemporaryCpgOutputFile(cpgFilePrefix: String): File = {
    val cpgFile = File.createTempFile(cpgFilePrefix, cpgFileSuffix)
    cpgFile.deleteOnExit()
    cpgFile
  }

  private def stdOutHandler: String => Unit = { out =>
    if (!port.isDone && out.startsWith(marker)) {
      port.complete(Integer.parseInt(out.substring(marker.length + 1)))
    } else {
      System.out.println(out)
    }
  }

  private def stdErrHandler: String => Unit = { out =>
    System.err.println(out)
  }

  private def start(): Unit = this.synchronized {
    if (serverProcess.isEmpty) {
      val process =
        Process(List(fullExecutablePath.toString, ".", "--server", "--server-timeout-minutes=1"))
          .run(ProcessLogger(stdOutHandler, stdErrHandler))

      while (process.isAlive && !port.isDone) {
        try {
          port.get(100, TimeUnit.MILLISECONDS)
        } catch {
          case _: TimeoutException =>
        }
      }
      assert(process.isAlive())
      serverProcess = Some(process)
    }
  }

  final def shutdown(): Unit = this.synchronized {
    serverProcess.foreach(_.destroy())
    serverProcess = None
  }

  private def parseArguments(args: Seq[String]): Seq[(String, Option[String])] = args.toList match {
    case Nil => Seq.empty
    case argument :: value :: rest if argument.startsWith("--") && !value.startsWith("-") =>
      (argument.stripPrefix("--") -> Some(value)) +: parseArguments(rest)
    case argument :: rest =>
      (argument.stripPrefix("--") -> None) +: parseArguments(rest)
  }

  def executeRequest(sourceCodeDir: File, extraArgs: String*): (Int, String, File) = {
    start()

    val cpgFile = newTemporaryCpgOutputFile("cpg")
    cpgFile.deleteOnExit()

    val arguments =
      Seq(
        "input"                        -> Some(sourceCodeDir.toString),
        "output"                       -> Some(cpgFile.getAbsolutePath),
        "enable-early-schema-checking" -> None
      ) ++ parseArguments(extraArgs)

    val client  = FrontendHTTPClient(port.getNow(-1))
    val request = client.buildRequest(arguments*)
    client.sendRequest(request) match {
      case Failure(exception) => (1, exception.getMessage, cpgFile)
      case Success(out)       => (0, out, cpgFile)
    }
  }
}
