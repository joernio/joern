package io.joern.console

import os.{Path, pwd}
import ammonite.util.{Colors, Res}
import better.files._
import io.joern.console.cpgqlserver.CPGQLServer
import io.joern.console.embammonite.EmbeddedAmmonite

case class Config(
  scriptFile: Option[Path] = None,
  command: Option[String] = None,
  params: Map[String, String] = Map.empty,
  additionalImports: List[Path] = Nil,
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
  serverAuthUsername: String = "",
  serverAuthPassword: String = "",
  nocolors: Boolean = false,
  cpgToLoad: Option[File] = None,
  forInputPath: Option[String] = None,
  frontendArgs: Array[String] = Array.empty
)

/** Base class for Ammonite Bridge. Nothing to see here, move along.
  */
trait BridgeBase {

  protected def parseConfig(args: Array[String]): Config = {
    implicit def pathRead: scopt.Read[Path] =
      scopt.Read.stringRead
        .map(Path(_, pwd)) // support both relative and absolute paths

    val parser = new scopt.OptionParser[Config]("(joern|ocular)") {
      override def errorOnUnknownArgument = false

      note("Script execution")

      opt[Path]("script")
        .action((x, c) => c.copy(scriptFile = Some(x)))
        .text("path to script file: will execute and exit")

      opt[Map[String, String]]('p', "params")
        .valueName("k1=v1,k2=v2")
        .action((x, c) => c.copy(params = x))
        .text("key values for script")

      opt[Seq[Path]]("import")
        .valueName("script1.sc,script2.sc,...")
        .action((x, c) => c.copy(additionalImports = x.toList))
        .text("import additional additional script(s): will execute and keep console open")

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
        .action((x, c) => c.copy(serverAuthUsername = x))
        .text("Basic auth username for the CPGQL server")

      opt[String]("server-auth-password")
        .action((x, c) => c.copy(serverAuthPassword = x))
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

      help("help")
        .text("Print this help text")
    }

    // note: if config is really `None` an error message would have been displayed earlier
    parser.parse(args, Config()).get
  }

  protected def runAmmonite(config: Config, slProduct: SLProduct = OcularProduct): Unit = {
    if (config.listPlugins) {
      listPluginsAndLayerCreators(config, slProduct)
    } else if (config.addPlugin.isDefined) {
      new PluginManager(InstallConfig().rootPath).add(config.addPlugin.get)
    } else if (config.rmPlugin.isDefined) {
      new PluginManager(InstallConfig().rootPath).rm(config.rmPlugin.get)
    } else {
      config.scriptFile match {
        case None =>
          if (config.server) {
            startHttpServer(config)
          } else if (config.pluginToRun.isDefined) {
            runPlugin(config, slProduct.name)
          } else {
            startInteractiveShell(config, slProduct)
          }
        case Some(scriptFile) =>
          runScript(scriptFile, config)
      }
    }
  }

  private def listPluginsAndLayerCreators(config: Config, slProduct: SLProduct): Unit = {
    println("Installed plugins:")
    println("==================")
    new PluginManager(InstallConfig().rootPath).listPlugins().foreach(println)
    println("Available layer creators")
    println()
    val code =
      """
        |println(run)
        |
        |""".stripMargin
    withTemporaryScript(code, slProduct.name) { file =>
      runScript(os.Path(file.path.toString), config)
    }
  }

  private def withTemporaryScript(code: String, prefix: String)(f: File => Unit): Unit = {
    File.usingTemporaryDirectory(prefix + "-bundle") { dir =>
      val file = (dir / "script.sc")
      file.write(code)
      f(file)
    }
  }

  private def runPlugin(config: Config, productName: String): Unit = {
    if (config.src.isEmpty) {
      println("You must supply a source directory with the --src flag")
      return
    }

    val bundleName = config.pluginToRun.get
    val src        = better.files.File(config.src.get).path.toAbsolutePath.toString
    val language = config.language.getOrElse(
      io.joern.console.cpgcreation
        .guessLanguage(src)
        .map { x =>
          val lang = x.toLowerCase
          // TODO we should eventually rename the languages in the
          // spec to `OLDC` and `C` at which point the match below
          // is no longer required.
          lang match {
            case "newc" => "c"
            case "c"    => "oldc"
            case _      => lang
          }
        }
        .getOrElse("c")
    )
    val storeCode = if (config.store) { "save" }
    else { "" }
    val runDataflow = if (productName == "ocular") { "run.dataflow" }
    else { "run.ossdataflow" }
    val argsString = config.frontendArgs match {
      case Array() => ""
      case args =>
        val quotedArgs = args.map { arg =>
          "\"" ++ arg ++ "\""
        }
        val argsString = quotedArgs.mkString(", ")
        s", args=List($argsString)"
    }
    val code = s"""
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

    withTemporaryScript(code, productName) { file =>
      runScript(os.Path(file.path.toString), config)
    }
  }

  private def startInteractiveShell(config: Config, slProduct: SLProduct) = {
    val configurePPrinterMaybe =
      if (config.nocolors) ""
      else """val originalPPrinter = repl.pprinter()
             |repl.pprinter.update(io.joern.console.pprinter.create(originalPPrinter))
             |""".stripMargin

    val replConfig = List(
      "repl.prompt() = \"" + promptStr() + "\"",
      configurePPrinterMaybe,
      "implicit val implicitPPrinter = repl.pprinter()",
      "banner()"
    ) ++ config.cpgToLoad.map { cpgFile =>
      "importCpg(\"" + cpgFile + "\")"
    } ++ config.forInputPath.map { name =>
      s"""
        |openForInputPath(\"$name\")
        |""".stripMargin
    }
    ammonite
      .Main(
        predefCode = predefPlus(additionalImportCode(config) ++ replConfig ++ shutdownHooks),
        welcomeBanner = None,
        storageBackend = new StorageBackend(slProduct),
        remoteLogging = false,
        colors = ammoniteColors(config)
      )
      .run()
  }

  private def startHttpServer(config: Config): Unit = {
    val predef   = predefPlus(additionalImportCode(config))
    val ammonite = new EmbeddedAmmonite(predef)
    ammonite.start()
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      ammonite.shutdown()
    }))
    val server = new CPGQLServer(
      ammonite,
      config.serverHost,
      config.serverPort,
      config.serverAuthUsername,
      config.serverAuthPassword
    )
    println("Starting CPGQL server ...")
    try {
      server.main(Array.empty)
    } catch {
      case _: java.net.BindException => {
        println("Could not bind socket for CPGQL server, exiting.")
        ammonite.shutdown()
        System.exit(1)
      }
      case e: Throwable => {
        println("Unhandled exception thrown while attempting to start CPGQL server: ")
        println(e.getMessage)
        println("Exiting.")

        ammonite.shutdown()
        System.exit(1)
      }
    }
  }

  private def runScript(scriptFile: Path, config: Config) = {
    val isEncryptedScript = scriptFile.ext == "enc"
    System.err.println(s"executing $scriptFile with params=${config.params}")
    val scriptArgs: Seq[String] = {
      val commandArgs   = config.command.toList
      val parameterArgs = config.params.flatMap { case (key, value) => Seq(s"--$key", value) }
      commandArgs ++ parameterArgs
    }
    val actualScriptFile =
      if (isEncryptedScript) decryptedScript(scriptFile)
      else scriptFile
    val importCpgCode = config.cpgToLoad.map { cpgFile =>
      "importCpg(\"" + cpgFile + "\")"
    }.toList ++ config.forInputPath.map { name =>
      s"""
         |openForInputPath(\"$name\")
         |""".stripMargin
    }
    ammonite
      .Main(
        predefCode = predefPlus(additionalImportCode(config) ++ importCpgCode ++ shutdownHooks),
        remoteLogging = false,
        colors = ammoniteColors(config)
      )
      .runScript(actualScriptFile, scriptArgs)
      ._1 match {
      case Res.Success(r) =>
        System.err.println(s"script finished successfully")
        System.err.println(r)
      case Res.Failure(msg) =>
        throw new AssertionError(s"script failed: $msg")
      case Res.Exception(e, msg) =>
        throw new AssertionError(s"script errored: $msg", e)
      case _ => ???
    }
    /* minimizing exposure time by deleting the decrypted script straight away */
    if (isEncryptedScript) actualScriptFile.toIO.delete
  }

  private def additionalImportCode(config: Config): List[String] =
    config.additionalImports.flatMap { importScript =>
      val file = importScript.toIO
      assert(file.canRead, s"unable to read $file")
      readScript(file.toScala)
    }

  private def ammoniteColors(config: Config) =
    if (config.nocolors) Colors.BlackWhite
    else Colors.Default

  /** Override this method to implement script decryption
    */
  protected def decryptedScript(scriptFile: Path): Path = {
    scriptFile
  }

  private def readScript(scriptFile: File): List[String] = {
    val code = scriptFile.lines.toList
    println(s"importing $scriptFile (${code.size} lines)")
    code
  }

  protected def predefPlus(lines: List[String]): String

  protected def shutdownHooks: List[String]

  protected def promptStr(): String

}
