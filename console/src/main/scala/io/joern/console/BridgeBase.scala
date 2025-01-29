package io.joern.console

import better.files.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.sarif.SarifConfig
import org.apache.commons.text.StringEscapeUtils
import replpp.scripting.ScriptRunner

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

case class Config(
  scriptFile: Option[Path] = None,
  command: Option[String] = None,
  params: Map[String, String] = Map.empty,
  predefFiles: Seq[Path] = Nil,
  runBefore: Seq[String] = Nil,
  additionalClasspathEntries: Seq[String] = Seq.empty,
  addPlugin: Option[String] = None,
  rmPlugin: Option[String] = None,
  pluginToRun: Option[String] = None,
  listPlugins: Boolean = false,
  src: Option[String] = None,
  language: Option[String] = None,
  overwrite: Boolean = false,
  store: Boolean = false,
  server: Boolean = false,
  serverHost: String = "localhost",
  serverPort: Int = 8080,
  serverAuthUsername: Option[String] = None,
  serverAuthPassword: Option[String] = None,
  nocolors: Boolean = false,
  cpgToLoad: Option[File] = None,
  forInputPath: Option[String] = None,
  frontendArgs: Array[String] = Array.empty,
  verbose: Boolean = false,
  dependencies: Seq[String] = Seq.empty,
  resolvers: Seq[String] = Seq.empty,
  maxHeight: Option[Int] = None
)

/** Base class for ReplBridge, split by topic into multiple self types.
  */
trait BridgeBase extends InteractiveShell with ScriptExecution with PluginHandling with ServerHandling {

  def applicationName: String

  protected def parseConfig(args: Array[String]): Config = {
    val parser = new scopt.OptionParser[Config](applicationName) {
      override def errorOnUnknownArgument = false

      note("Script execution")

      opt[Path]("script")
        .action((x, c) => c.copy(scriptFile = Some(x)))
        .text("path to script file: will execute and exit")

      opt[String]("param")
        .valueName("param1=value1")
        .unbounded()
        .optional()
        .action { (x, c) =>
          x.split("=", 2) match {
            case Array(key, value) => c.copy(params = c.params + (key -> value))
            case _                 => throw new IllegalArgumentException(s"unable to parse param input $x")
          }
        }
        .text("key/value pair for main function in script - may be passed multiple times")

      opt[Path]("import")
        .valueName("script1.sc")
        .unbounded()
        .optional()
        .action((x, c) => c.copy(predefFiles = c.predefFiles :+ x))
        .text("given source files will be compiled and added to classpath - this may be passed multiple times")

      opt[String]("runBefore")
        .valueName("'import Int.MaxValue'")
        .unbounded()
        .optional()
        .action((x, c) => c.copy(runBefore = c.runBefore :+ x))
        .text("given code will be executed on startup - this may be passed multiple times")

      opt[String]("classpathEntry")
        .valueName("path/to/classpath")
        .unbounded()
        .optional()
        .action((x, c) => c.copy(additionalClasspathEntries = c.additionalClasspathEntries :+ x))
        .text("additional classpath entries - may be passed multiple times")

      opt[String]('d', "dep")
        .valueName("com.michaelpollmeier:versionsort:1.0.7")
        .unbounded()
        .optional()
        .action((x, c) => c.copy(dependencies = c.dependencies :+ x))
        .text(
          "add artifacts (including transitive dependencies) for given maven coordinate to classpath - may be passed multiple times"
        )

      opt[String]('r', "repo")
        .valueName("https://repository.apache.org/content/groups/public/")
        .unbounded()
        .optional()
        .action((x, c) => c.copy(resolvers = c.resolvers :+ x))
        .text("additional repositories to resolve dependencies - may be passed multiple times")

      opt[String]("command")
        .action((x, c) => c.copy(command = Some(x)))
        .text("select one of multiple @main methods")

      note("Plugin Management")

      opt[String]("add-plugin")
        .action((x, c) => c.copy(addPlugin = Some(x)))
        .text("Plugin zip file to add to the installation")

      opt[String]("remove-plugin")
        .action((x, c) => c.copy(rmPlugin = Some(x)))
        .text("Name of plugin to remove from the installation")

      opt[Unit]("plugins")
        .action((_, c) => c.copy(listPlugins = true))
        .text("List available plugins and layer creators")

      opt[String]("run")
        .action((x, c) => c.copy(pluginToRun = Some(x)))
        .text("Run layer creator. Get a list via --plugins")

      opt[String]("src")
        .action((x, c) => c.copy(src = Some(x)))
        .text("Source code directory to run layer creator on")

      opt[String]("language")
        .action((x, c) => c.copy(language = Some(x)))
        .text("Language to use in CPG creation")

      opt[Unit]("overwrite")
        .action((_, c) => c.copy(overwrite = true))
        .text("Overwrite CPG if it already exists")

      opt[Unit]("store")
        .action((_, c) => c.copy(store = true))
        .text("Store graph changes made by layer creator")

      note("REST server mode")

      opt[Unit]("server")
        .action((_, c) => c.copy(server = true))
        .text("run as HTTP server")

      opt[String]("server-host")
        .action((x, c) => c.copy(serverHost = x))
        .text("Hostname on which to expose the CPGQL server")

      opt[Int]("server-port")
        .action((x, c) => c.copy(serverPort = x))
        .text("Port on which to expose the CPGQL server")

      opt[String]("server-auth-username")
        .action((x, c) => c.copy(serverAuthUsername = Option(x)))
        .text("Basic auth username for the CPGQL server")

      opt[String]("server-auth-password")
        .action((x, c) => c.copy(serverAuthPassword = Option(x)))
        .text("Basic auth password for the CPGQL server")

      note("Misc")

      arg[java.io.File]("<cpg.bin>")
        .optional()
        .action((x, c) => c.copy(cpgToLoad = Some(x.toScala)))
        .text("CPG to load")

      opt[String]("for-input-path")
        .action((x, c) => c.copy(forInputPath = Some(x)))
        .text("Open CPG for given input path - overrides <cpg.bin>")

      opt[Unit]("nocolors")
        .action((_, c) => c.copy(nocolors = true))
        .text("turn off colors")

      opt[Unit]("verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("enable verbose output (predef, resolved dependency jars, ...)")

      opt[Int]("maxHeight")
        .action((x, c) => c.copy(maxHeight = Some(x)))
        .text("Maximum number lines to print before output gets truncated (default: no limit)")

      help("help")
        .text("Print this help text")
    }

    // note: if config is really `None` an error message would have been displayed earlier
    parser.parse(args, Config()).get
  }

  /** Entry point for Joern's integrated REPL and plugin manager */
  protected def run(config: Config): Unit = {
    if (config.listPlugins) {
      printPluginsAndLayerCreators(config)
    } else if (config.addPlugin.isDefined) {
      new PluginManager(InstallConfig().rootPath).add(config.addPlugin.get)
    } else if (config.rmPlugin.isDefined) {
      new PluginManager(InstallConfig().rootPath).rm(config.rmPlugin.get)
    } else if (config.scriptFile.isDefined) {
      val scriptReturn = runScript(config)
      if (scriptReturn.isFailure) {
        println(scriptReturn.failed.get.getMessage)
        System.exit(1)
      }
    } else if (config.server) {
      GlobalReporting.enable()
      startHttpServer(config)
    } else if (config.pluginToRun.isDefined) {
      runPlugin(config)
    } else {
      startInteractiveShell(config)
    }
  }

  /** code that is executed on startup */
  protected def runBeforeCode: Seq[String]

  protected def buildRunBeforeCode(config: Config): Seq[String] = {
    val builder = Seq.newBuilder[String]
    builder ++= runBeforeCode
    config.cpgToLoad.foreach { cpgFile =>
      builder += s"""importCpg("$cpgFile")"""
    }
    config.forInputPath.foreach { name =>
      builder += s"""openForInputPath("$name")""".stripMargin
    }
    builder ++= config.runBefore
    builder.result()
  }

  protected def greeting: String

  protected def promptStr: String

  protected def onExitCode: String
}

trait InteractiveShell { this: BridgeBase =>
  protected def startInteractiveShell(config: Config) = {
    replpp.InteractiveShell.run(
      replpp.Config(
        predefFiles = config.predefFiles,
        runBefore = buildRunBeforeCode(config),
        nocolors = config.nocolors,
        verbose = config.verbose,
        classpathConfig = replpp.Config
          .ForClasspath(
            additionalClasspathEntries = config.additionalClasspathEntries,
            inheritClasspath = true,
            dependencies = config.dependencies,
            resolvers = config.resolvers
          ),
        greeting = Option(greeting),
        prompt = Option(promptStr),
        onExitCode = Option(onExitCode),
        maxHeight = config.maxHeight
      )
    )
  }

}

trait ScriptExecution { this: BridgeBase =>

  def runScript(config: Config): Try[Unit] = {
    val scriptFile = config.scriptFile.getOrElse(throw new AssertionError("no script file configured"))
    if (!Files.exists(scriptFile)) {
      Try(throw new AssertionError(s"given script file `$scriptFile` does not exist"))
    } else {
      val scriptReturn = ScriptRunner.exec(
        replpp.Config(
          predefFiles = config.predefFiles,
          runBefore = buildRunBeforeCode(config),
          scriptFile = Option(scriptFile),
          command = config.command,
          params = config.params,
          verbose = config.verbose,
          classpathConfig = replpp.Config
            .ForClasspath(inheritClasspath = true, dependencies = config.dependencies, resolvers = config.resolvers)
        )
      )
      if (config.verbose && scriptReturn.isFailure) {
        println(scriptReturn.failed.get.getMessage)
      }
      scriptReturn
    }
  }
}

trait PluginHandling { this: BridgeBase =>

  /** Print a summary of the available plugins and layer creators to the terminal.
    */
  protected def printPluginsAndLayerCreators(config: Config): Unit = {
    println("Installed plugins:")
    println("==================")
    new PluginManager(InstallConfig().rootPath).listPlugins().foreach(println)
    println("Available layer creators")
    println()
    withTemporaryScript(codeToListPlugins()) { file =>
      runScript(config.copy(scriptFile = Some(file.path))).get
    }
  }

  private def codeToListPlugins(): String = {
    """
      |println(run)
      |
      |""".stripMargin
  }

  /** Run plugin by generating a temporary script based on the given config and execute the script */
  protected def runPlugin(config: Config): Unit = {
    if (config.src.isEmpty) {
      println("You must supply a source directory with the --src flag")
      return
    }
    val code = loadOrCreateCpg(config, applicationName)
    withTemporaryScript(code) { file =>
      runScript(config.copy(scriptFile = Some(file.path))).get
    }
  }

  /** Create a command that loads an existing CPG or creates it, based on the given `config`.
    */
  private def loadOrCreateCpg(config: Config, productName: String): String = {

    val bundleName = config.pluginToRun.get
    val srcRaw     = better.files.File(config.src.get).path.toAbsolutePath.toString
    val src        = StringEscapeUtils.escapeJava(srcRaw)
    val language   = languageFromConfig(config, src)

    val storeCode = if (config.store) { "save" }
    else { "" }
    val runDataflow = if (productName == "ocular") { "run.dataflow" }
    else { "run.ossdataflow" }
    val argsString = argsStringFromConfig(config)

    s"""
       | if (${config.overwrite} || !workspace.projectExists("$src")) {
       |   workspace.projects
       |   .filter(_.inputPath == "$src")
       |   .map(_.name).foreach(n => workspace.removeProject(n))
       |   importCode.$language("$src"$argsString)
       |   $runDataflow
       |   save
       | } else {
       |    println("Using existing CPG - Use `--overwrite` if this is not what you want")
       |    openForInputPath(\"$src\")
       | }
       | run.$bundleName
       | $storeCode
       |""".stripMargin

  }

  private def languageFromConfig(config: Config, src: String): String = {
    config.language.getOrElse(
      io.joern.console.cpgcreation
        .guessLanguage(src)
        .map {
          case Languages.C | Languages.NEWC => "c"
          case Languages.JAVA               => "jvm"
          case Languages.JAVASRC            => "java"
          case lang                         => lang.toLowerCase
        }
        .getOrElse("c")
    )
  }

  private def argsStringFromConfig(config: Config): String = {
    config.frontendArgs match {
      case Array() => ""
      case args =>
        val quotedArgs = args.map { arg =>
          "\"" ++ arg ++ "\""
        }
        val argsString = quotedArgs.mkString(", ")
        s", args=List($argsString)"
    }
  }

  private def withTemporaryScript(code: String)(f: File => Unit): Unit = {
    File.usingTemporaryDirectory(applicationName + "-bundle") { dir =>
      val file = dir / "script.sc"
      file.write(code)
      f(file)
    }
  }

}

trait ServerHandling { this: BridgeBase =>

  protected def startHttpServer(config: Config): Unit = {
    val baseConfig = replpp.Config(
      predefFiles = config.predefFiles,
      runBefore = buildRunBeforeCode(config),
      verbose = true, // always print what's happening - helps debugging
      classpathConfig = replpp.Config
        .ForClasspath(inheritClasspath = true, dependencies = config.dependencies, resolvers = config.resolvers)
    )

    replpp.server.ReplServer.startHttpServer(
      replpp.server.Config(
        baseConfig,
        serverHost = config.serverHost,
        serverPort = config.serverPort,
        serverAuthUsername = config.serverAuthUsername,
        serverAuthPassword = config.serverAuthPassword
      )
    )
  }

}
