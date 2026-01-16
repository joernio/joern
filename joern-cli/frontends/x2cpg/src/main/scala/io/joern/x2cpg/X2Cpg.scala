package io.joern.x2cpg

import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.server.FrontendHTTPServer
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}
import io.shiftleft.semanticcpg.utils.FileUtil
import org.slf4j.LoggerFactory
import scopt.{DefaultOParserSetup, OParser, OParserSetup}

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.concurrent.ExecutorService
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object X2CpgConfig {
  def defaultOutputPath: String = ""

  final case class GenericConfig(
    inputPath: String = "",
    outputPath: String = X2CpgConfig.defaultOutputPath,
    serverMode: Boolean = false,
    serverTimeoutSeconds: Option[Long] = None,
    defaultIgnoredFilesRegex: Seq[Regex] = Seq.empty,
    ignoredFilesRegex: Regex = "".r,
    ignoredFiles: Seq[String] = Seq.empty,
    schemaValidation: ValidationMode = ValidationMode.Disabled,
    disableFileContent: Boolean = true
  ) {}

}

trait X2CpgConfig[T <: X2CpgConfig[?]] {
  final type OwnType = T

  protected def genericConfig: X2CpgConfig.GenericConfig
  final def inputPath: String                    = genericConfig.inputPath
  final def outputPath: String                   = genericConfig.outputPath
  final def serverMode: Boolean                  = genericConfig.serverMode
  final def serverTimeoutSeconds: Option[Long]   = genericConfig.serverTimeoutSeconds
  final def defaultIgnoredFilesRegex: Seq[Regex] = genericConfig.defaultIgnoredFilesRegex
  final def ignoredFilesRegex: Regex             = genericConfig.ignoredFilesRegex
  final def ignoredFiles: Seq[String]            = genericConfig.ignoredFiles
  final def schemaValidation: ValidationMode     = genericConfig.schemaValidation
  final def disableFileContent: Boolean          = genericConfig.disableFileContent

  protected def withGenericConfig(value: X2CpgConfig.GenericConfig): OwnType

  final def withInputPath(inputPath: String): OwnType = {
    withGenericConfig(genericConfig.copy(inputPath = Paths.get(inputPath).toAbsolutePath.normalize().toString))
  }

  final def withOutputPath(x: String): OwnType = {
    withGenericConfig(genericConfig.copy(outputPath = x))
  }

  final def withServerMode(x: Boolean): OwnType = {
    withGenericConfig(genericConfig.copy(serverMode = x))
  }

  final def withServerTimeoutSeconds(x: Long): OwnType = {
    withGenericConfig(genericConfig.copy(serverTimeoutSeconds = Some(x)))
  }

  final def withDefaultIgnoredFilesRegex(x: Seq[Regex]): OwnType = {
    withGenericConfig(genericConfig.copy(defaultIgnoredFilesRegex = x))
  }

  final def withIgnoredFilesRegex(x: String): OwnType = {
    withGenericConfig(genericConfig.copy(ignoredFilesRegex = x.r))
  }

  final def withIgnoredFiles(x: Seq[String]): OwnType = {
    def createPathForIgnore(ignore: String): String = {
      val path = Paths.get(ignore)
      if (path.isAbsolute) {
        path.toString
      } else {
        Paths.get(inputPath, ignore).toAbsolutePath.normalize().toString
      }
    }

    withGenericConfig(genericConfig.copy(ignoredFiles = x.map(createPathForIgnore)))
  }

  final def withSchemaValidation(value: ValidationMode): OwnType = {
    withGenericConfig(genericConfig.copy(schemaValidation = value))
  }

  final def withDisableFileContent(value: Boolean): OwnType = {
    withGenericConfig(genericConfig.copy(disableFileContent = value))
  }
}

/** Enables the configuration to specify if dependencies should be downloaded for additional symbol information.
  */
trait DependencyDownloadConfig { this: X2CpgConfig[?] =>

  def withDownloadDependencies(value: Boolean): OwnType

}

object DependencyDownloadConfig {
  def parserOptions[R <: X2CpgConfig[R] & DependencyDownloadConfig]: OParser[?, R] = {
    val builder = OParser.builder[R]
    import builder.*
    OParser.sequence(
      opt[Unit]("download-dependencies")
        .text("Download the dependencies of the target project and use their symbols to resolve types.")
        .action((_, c) => c.withDownloadDependencies(true))
    )
  }
}

object X2CpgMain {
  private val logger = LoggerFactory.getLogger(classOf[X2CpgMain.type])
}

trait SingleThreadedFrontend { this: X2CpgMain =>
  override protected def executor: ExecutorService = FrontendHTTPServer.singleThreadExecutor()
}

/** Base class for `Main` classes of CPG frontends.
  *
  * Main classes that inherit from this base class parse the command line, exiting with an error code if this does not
  * succeed. On success, the method `run` is called, which evaluates, given a frontend and a configuration, creates the
  * CPG and stores it on disk.
  *
  * @param cmdLineParser
  *   parser for command line arguments
  * @param frontend
  *   the frontend to use for CPG creation
  */
abstract class X2CpgMain(val frontend: X2CpgFrontend, val cmdLineParser: OParser[Unit, frontend.ConfigType]) {
  import X2CpgMain.*

  private def logVersionAndArgs(args: Array[String]): Unit = {
    val frontendName = frontend.getClass.getSimpleName.stripSuffix("$")
    val joernVersion =
      // We only have a proper version there if joern was build using sbt assembly. Otherwise, it might be null.
      Option(frontend.getClass.getPackage.getImplementationVersion).map(v => s"v$v").getOrElse("local build")
    val logText = s"Executing $frontendName ($joernVersion) with arguments: ${args.mkString(" ")}"
    logger.debug(logText)
  }

  private def logOutputPath(outputPath: String): Unit = {
    if (X2CpgConfig.defaultOutputPath == outputPath) {
      // We only log the output path of no explicit path was given by the user.
      // Otherwise, the user obviously knows the path.
      logger.info(s"The resulting CPG will be stored at ${Paths.get(outputPath).toString}")
    }
  }

  /** ExecutorService used to execute HTTP requests.
    *
    * This can be overridden to switch between single-threaded and multi-threaded execution. By default, it uses the
    * cached thread pool executor from `FrontendHTTPServer`.
    */
  protected def executor: ExecutorService = FrontendHTTPServer.defaultExecutor()

  val server: FrontendHTTPServer = new FrontendHTTPServer(
    executor,
    arguments => {
      val config = X2Cpg
        .parseCommandLine(arguments, cmdLineParser, frontend.defaultConfig)
        .getOrElse(frontend.defaultConfig)
      frontend.run(config)
    }
  )

  /** method that evaluates frontend with configuration
    */
  def run(config: frontend.ConfigType): Unit = {
    if (config.serverMode) {
      try {
        val port = server.startup()
        println(s"FrontendHTTPServer started on port $port")

        config.serverTimeoutSeconds match {
          case Some(timeout) => server.stopServerAfterTimeout(timeout)
          case None          => Thread.sleep(Long.MaxValue)
        }
      } finally {
        frontend.close()
      }
    } else {
      frontend.run(config)
    }
  }

  def main(args: Array[String]): Unit = {
    logVersionAndArgs(args)
    X2Cpg.parseCommandLine(args, cmdLineParser, frontend.defaultConfig) match {
      case Some(config) if !Environment.pathExists(config.inputPath) =>
        logger.warn(s"Given path '${config.inputPath}' does not exist")
        System.exit(1)
      case Some(config) =>
        try {
          logOutputPath(config.outputPath)
          run(config)
        } catch {
          case ex: Throwable =>
            ex.printStackTrace()
            System.exit(1)
        } finally {
          frontend.close()
        }
      case None =>
        println("Error parsing the command line")
        System.exit(1)
    }
  }

}

/** Trait that represents a CPG generator, where T is the frontend configuration class.
  */
trait X2CpgFrontend extends AutoCloseable {
  type ConfigType <: X2CpgConfig[ConfigType]
  val defaultConfig: ConfigType

  /** Create a CPG according to given configuration. Returns CPG wrapped in a `Try`, making it possible to detect and
    * inspect exceptions in CPG generation. To be provided by the frontend.
    */
  def createCpg(config: ConfigType): Try[Cpg]

  /** Create CPG according to given configuration, printing errors to the console if they occur. The CPG is closed and
    * not returned.
    */
  @throws[Throwable]("if createCpg throws any Throwable")
  def run(config: ConfigType): Unit = {
    createCpg(config).map { cpg =>
      cpg.close() // persists to disk
    } match {
      case Failure(exception) =>
        // We explicitly rethrow the exception so that every frontend will
        // terminate with exit code 1 if there was an exception during createCpg.
        // Frontend maintainer may want to catch that RuntimeException on their end
        // to add custom error handling.
        throw exception
      case Success(_) => // this is fine
    }
  }

  /** Create a CPG with default overlays according to given configuration
    */
  def createCpgWithOverlays(config: ConfigType): Try[Cpg] = {
    val maybeCpg = createCpg(config)
    maybeCpg.map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }
  }

  /** For frontends that create and manage resources during AST generation, they can clean up these resources here.
    */
  override def close(): Unit = {}
}

object X2Cpg {

  private val logger = LoggerFactory.getLogger(X2Cpg.getClass)

  private val oParserSetup: OParserSetup = new DefaultOParserSetup {
    override def showUsageOnError: Option[Boolean] = Some(true)
  }

  /** Parse commands line arguments in `args` using an X2Cpg command line parser, extended with the frontend specific
    * options in `frontendSpecific` with the initial configuration set to `initialConf`. On success, the configuration
    * is returned wrapped into an Option. On failure, error messages are printed and, `None` is returned.
    */
  def parseCommandLine[R <: X2CpgConfig[R]](
    args: Array[String],
    frontendSpecific: OParser[?, R],
    initialConf: R
  ): Option[R] = {
    val parser = commandLineParser(frontendSpecific)
    OParser.parse(parser, args, initialConf, oParserSetup)
  }

  /** Create a command line parser that can be extended to add options specific for the frontend.
    */
  private def commandLineParser[R <: X2CpgConfig[R]](frontendSpecific: OParser[?, R]): OParser[?, R] = {
    val builder = OParser.builder[R]
    import builder.*
    OParser.sequence(
      arg[String]("input-dir")
        .text("source directory")
        .action((x, c) => c.withInputPath(x)),
      opt[String]("output")
        .abbr("o")
        .text("output filename")
        .action { (x, c) =>
          c.withOutputPath(x)
        },

      // previously this was supposed to be called with `,` as a separator,
      // e.g. `--exclude foo,bar` - which (among others) has the disadvantage
      // that under windows a `,` is treated as an argument separator
      // better: provide this argument multiple times, i.e. `--exclude foo --exclude bar`
      opt[Seq[String]]("exclude")
        .valueName("<file1>")
        .unbounded()
        .action { (x, c) => c.withIgnoredFiles(c.ignoredFiles ++ x) }
        .text("files or folders to exclude during CPG generation (paths relative to <input-dir> or absolute paths)"),
      opt[String]("exclude-regex")
        .action { (x, c) => c.withIgnoredFilesRegex(x) }
        .text("a regex specifying files to exclude during CPG generation (paths relative to <input-dir> are matched)"),
      opt[Unit]("no-default-exclude").action { (_, c) => c.withDefaultIgnoredFilesRegex(Nil) }.hidden(),
      opt[Unit]("enable-early-schema-checking")
        .action((_, c) => c.withSchemaValidation(ValidationMode.Enabled))
        .text("enables early schema validation during AST creation (disabled by default)"),
      opt[Unit]("enable-file-content")
        .action((_, c) => c.withDisableFileContent(false))
        .text(
          "add the raw source code to the content field of FILE nodes to allow for method source retrieval via offset fields (disabled by default)"
        ),
      opt[Unit]("server")
        .action((_, c) => c.withServerMode(true))
        .hidden()
        .text("runs this frontend in server mode (disabled by default)"),
      opt[Long]("server-timeout-minutes")
        .action((secs, c) => c.withServerTimeoutSeconds(secs * 60))
        .hidden()
        .text("timeout after which the server should terminate (use with --server)"),
      opt[Unit]("disable-file-content")
        .action((_, c) => c.withDisableFileContent(true))
        .hidden()
        .text("currently unused option that will replace enable-file-content"),
      help("help").text("display this help message"),
      frontendSpecific
    )
  }

  /** Create an empty CPG, backed by the file at `optionalOutputPath` or in-memory if `optionalOutputPath` is empty.
    */
  def newEmptyCpg(optionalOutputPath: Option[String] = None): Cpg = {
    optionalOutputPath match {
      case Some(outputPath) =>
        lazy val outFile = Paths.get(outputPath)
        if (outputPath != "" && Files.exists(outFile)) {
          logger.info("Output file exists, removing: " + outputPath)
          FileUtil.delete(outFile)
        }
        Cpg.withStorage(outFile)
      case None => Cpg.empty
    }
  }

  /** Apply function `applyPasses` to a newly created CPG. The CPG is wrapped in a `Try` and returned. On failure, the
    * CPG is ensured to be closed.
    */
  def withNewEmptyCpg[T <: X2CpgConfig[T]](outPath: String, config: T)(applyPasses: (Cpg, T) => Unit): Try[Cpg] = {
    val outputPath = if (outPath != "") Some(outPath) else None
    Try {
      val cpg = newEmptyCpg(outputPath)
      Try {
        applyPasses(cpg, config)
      } match {
        case Success(_) => cpg
        case Failure(exception) =>
          cpg.close()
          throw exception
      }
    }
  }

  /** For a CPG generated by a frontend, run the default passes that turn a frontend-CPG into a complete CPG.
    */
  def applyDefaultOverlays(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    defaultOverlayCreators().foreach { creator =>
      creator.run(context)
    }
  }

  /** This should be the only place where we define the list of default overlays.
    */
  def defaultOverlayCreators(): List[LayerCreator] = {
    List(new Base(), new ControlFlow(), new TypeRelations(), new CallGraph())
  }

  /** Write `sourceCode` to a temporary file inside a temporary directory. The prefix for the temporary directory is
    * given by `tmpDirPrefix`. The suffix for the temporary file is given by `suffix`. Both file and directory are
    * deleted on exit.
    */
  def writeCodeToFile(sourceCode: String, tmpDirPrefix: String, suffix: String): java.io.File = {
    val tmpDir = Files.createTempDirectory(tmpDirPrefix).toFile
    tmpDir.deleteOnExit()
    val codeFile = java.io.File.createTempFile("Test", suffix, tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(sourceCode); close() }
    tmpDir
  }

  /** Strips surrounding quotation characters from a string.
    * @param str
    *   the target string.
    * @return
    *   the stripped string.
    */
  def stripQuotes(str: String): String = str.replaceAll("^(\"|')|(\"|')$", "")

}
