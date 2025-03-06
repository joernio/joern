package io.joern.joerncli

import io.joern.console.scan.{ScanPass, outputFindings}
import io.joern.console.{BridgeBase, DefaultArgumentProvider, Query, QueryDatabase}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.{NoSemantics, Semantics}
import io.joern.joerncli.JoernScan.getQueriesFromQueryDb
import io.joern.joerncli.Scan.{allTag, defaultTag}
import io.joern.joerncli.console.ReplBridge
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.{DefaultNodeExtensionFinder, NodeExtensionFinder}
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import org.json4s.native.Serialization
import org.json4s.{Formats, NoTypeHints}

import java.io.FileNotFoundException
import java.nio.file.{Files, NoSuchFileException, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object JoernScanConfig {
  val defaultDbVersion: String    = "latest"
  val defaultDumpQueryDestination = "/tmp/querydb.json"
}

case class JoernScanConfig(
  src: String = "",
  overwrite: Boolean = false,
  store: Boolean = false,
  dump: Boolean = false,
  dumpDestination: String = JoernScanConfig.defaultDumpQueryDestination,
  listQueryNames: Boolean = false,
  updateQueryDb: Boolean = false,
  queryDbVersion: String = JoernScanConfig.defaultDbVersion,
  maxCallDepth: Int = 2,
  names: String = "",
  tags: String = "",
  language: Option[String] = None,
  listLanguages: Boolean = false
)

object JoernScan extends BridgeBase {
  override val applicationName = "joern"

  val implementationVersion = getClass.getPackage.getImplementationVersion

  def main(args: Array[String]) = {
    val (scanArgs, frontendArgs) = CpgBasedTool.splitArgs(args)
    optionParser.parse(scanArgs, JoernScanConfig()).foreach { config =>
      run(config, frontendArgs)
    }
  }

  val optionParser = new scopt.OptionParser[JoernScanConfig]("joern-scan") {
    head(
      s"Creates a code property graph and scans it with queries from installed bundles.\nVersion: `$implementationVersion`"
    )
    help("help")
      .text("Prints this usage text")

    arg[String]("src")
      .text("source code directory to scan")
      .optional()
      .action((x, c) => c.copy(src = x))

    opt[Unit]("overwrite")
      .action((_, c) => c.copy(overwrite = true))
      .text("Overwrite CPG if it already exists")

    opt[Unit]("store")
      .action((_, c) => c.copy(store = true))
      .text("Store graph changes made by scanner")

    opt[Unit]("dump")
      .action((_, c) => c.copy(dump = true, dumpDestination = JoernScanConfig.defaultDumpQueryDestination))
      .text(s"Dump available queries to a file at `${JoernScanConfig.defaultDumpQueryDestination}`")

    opt[String]("dump-to")
      .action((x, c) => c.copy(dumpDestination = x, dump = true))
      .text("Dump available queries to a specific file")

    opt[Unit]("list-query-names")
      .action((_, c) => c.copy(listQueryNames = true))
      .text("Print a list of available query names")

    opt[Unit]("updatedb")
      .action((_, c) => c.copy(updateQueryDb = true))
      .text("Update query database")

    opt[String]("dbversion")
      .action((x, c) => c.copy(queryDbVersion = x))
      .text("Version of query database `updatedb`-operation installs")

    opt[String]("names")
      .action((x, c) => c.copy(names = x))
      .text("Filter queries used for scanning by name, comma-separated string")

    opt[String]("tags")
      .action((x, c) => c.copy(tags = x))
      .text("Filter queries used for scanning by tags, comma-separated string")

    opt[Int]("depth")
      .action((x, c) => c.copy(maxCallDepth = x))
      .text("Set call depth for interprocedural analysis")

    opt[String]("language")
      .action((x, c) => c.copy(language = Some(x)))
      .text("Source language")

    opt[Unit]("list-languages")
      .action((_, c) => c.copy(listLanguages = true))
      .text("List available language options")

    note(s"Args specified after the ${CpgBasedTool.ARGS_DELIMITER} separator will be passed to the front-end verbatim")
  }

  private def run(config: JoernScanConfig, frontendArgs: List[String]): Unit = {
    if (config.dump) {
      dumpQueriesAsJson(config.dumpDestination)
    } else if (config.listQueryNames) {
      listQueryNames()
    } else if (config.listLanguages) {
      listLanguages()
    } else if (config.updateQueryDb) {
      updateQueryDatabase(config.queryDbVersion)
    } else {
      runScanPlugin(config, frontendArgs)
    }
  }

  private def dumpQueriesAsJson(outFileName: String): Unit = {
    implicit val engineContext: EngineContext = EngineContext(NoSemantics)
    implicit val formats: AnyRef & Formats    = Serialization.formats(NoTypeHints)
    val queryDb                               = new QueryDatabase(new JoernDefaultArgumentProvider(0))
    better.files
      .File(outFileName)
      .write(Serialization.write(queryDb.allQueries))
    println(s"Queries written to: $outFileName")
  }

  private def listQueryNames(): Unit = {
    println(queryNames().sorted.mkString("\n"))
  }

  private def listLanguages(): Unit = {
    val s = new mutable.StringBuilder()
    s ++= "Available languages (case insensitive):\n"
    s ++= Languages.ALL.asScala.map(lang => s"- ${lang.toLowerCase}").mkString("\n")
    println(s.toString())
  }

  private def runScanPlugin(config: JoernScanConfig, frontendArgs: List[String]): Unit = {

    if (config.src == "") {
      println(optionParser.usage)
      return
    }

    if (queryNames().isEmpty) {
      downloadAndInstallQueryDatabase(config.queryDbVersion)
      System.exit(2)
    }

    Scan.defaultOpts.names = config.names.split(",").filterNot(_.isEmpty)
    Scan.defaultOpts.tags = config.tags.split(",").filterNot(_.isEmpty)
    Scan.defaultOpts.maxCallDepth = config.maxCallDepth

    val shellConfig = io.joern.console
      .Config()
      .copy(
        pluginToRun = Some("scan"),
        src = Some(config.src),
        overwrite = config.overwrite,
        store = config.store,
        language = config.language,
        frontendArgs = frontendArgs.toArray
      )
    run(shellConfig)
    println(s"Run `joern --for-input-path ${config.src}` to explore interactively")
  }

  private def queryNames(): List[String] = {
    implicit val engineContext: EngineContext = EngineContext(NoSemantics)
    getQueriesFromQueryDb(new JoernDefaultArgumentProvider(0)).map(_.name)
  }

  /** Obtain list of queries from query database, warning the user if the list is empty.
    */
  def getQueriesFromQueryDb(defaultArgumentProvider: DefaultArgumentProvider): List[Query] = {
    new QueryDatabase(defaultArgumentProvider).allQueries
  }

  private def updateQueryDatabase(version: String): Unit = {
    removeQueryDatabase()
    downloadAndInstallQueryDatabase(version)
  }

  def downloadAndInstallQueryDatabase(version: String = ""): Unit = {
    val actualVersion = Option(version).filter(_ != "").getOrElse(JoernScanConfig.defaultDbVersion)
    FileUtil.usingTemporaryDirectory("joern-scan") { dir =>
      val queryDbZipPath = downloadDefaultQueryDatabase(actualVersion, dir)
      addQueryDatabase(queryDbZipPath)
    }
  }

  private def downloadDefaultQueryDatabase(version: String, outDir: Path): String = {
    val url = urlForVersion(version)
    println(s"Downloading default query bundle from: $url")
    val r          = requests.get(url)
    val queryDbZip = outDir / "querydb.zip"
    val absPath    = queryDbZip.absolutePathAsString

    FileUtil.writeBytes(queryDbZip, r.bytes)

    println(s"Wrote: ${queryDbZip.size} bytes to $absPath")
    absPath
  }

  private def removeQueryDatabase(): Unit = {
    println("Removing current version of query database")
    val rmPluginConfig = io.joern.console
      .Config()
      .copy(rmPlugin = Some("querydb"))
    run(rmPluginConfig)
  }

  private def addQueryDatabase(absPath: String): Unit = {
    println("Adding updated version of query database")
    val addPluginConfig = io.joern.console
      .Config()
      .copy(addPlugin = Some(absPath))
    run(addPluginConfig)
  }

  private def urlForVersion(version: String): String = {
    if (version == "latest") {
      "https://github.com/joernio/joern/releases/latest/download/querydb.zip"
    } else {
      s"https://github.com/joernio/joern/releases/download/v$version/querydb.zip"
    }
  }

  override protected def runBeforeCode = ReplBridge.runBeforeCode
  override protected def promptStr     = ReplBridge.promptStr
  override protected def greeting      = ReplBridge.greeting
  override protected def onExitCode    = ReplBridge.onExitCode
}

object Scan {
  val overlayName = "scan"
  val description = "Joern Code Scanner"
  var defaultOpts = new ScanOptions(maxCallDepth = 2, names = Array[String](), tags = Array[String]())

  val defaultTag = "default"
  val allTag     = "all"
}

class ScanOptions(var maxCallDepth: Int, var names: Array[String], var tags: Array[String])
    extends LayerCreatorOptions {}

class Scan(options: ScanOptions)(implicit engineContext: EngineContext) extends LayerCreator {
  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  override val overlayName: String = Scan.overlayName
  override val description: String = Scan.description

  override def create(context: LayerCreatorContext): Unit = {
    val allQueries = getQueriesFromQueryDb(new JoernDefaultArgumentProvider(options.maxCallDepth))
    if (allQueries.isEmpty) {
      println("No queries found, you probably forgot to install a query database.")
      return
    }
    val queriesAfterFilter = filteredQueries(allQueries, options.names, options.tags)
    if (queriesAfterFilter.isEmpty) {
      println("No queries matched current filter selection (total number of queries: `" + allQueries.length + "`)")
      return
    }
    ScanPass(context.cpg, queriesAfterFilter).createAndApply()
    outputFindings(context.cpg)

  }

  protected def filteredQueries(queries: List[Query], names: Array[String], tags: Array[String]): List[Query] = {
    val filteredByName =
      if (names.length == 0) {
        queries
      } else {
        queries.filter { q =>
          names.contains(q.name)
        }
      }

    val filteredByTag =
      if (tags.length == 0 && names.length != 0) {
        filteredByName
      } else if (tags.length == 0) {
        filteredByName.filter(q => q.tags.contains(defaultTag))
      } else if (tags.sameElements(Array(allTag))) {
        filteredByName
      } else {
        val tagsSet = tags.toSet
        filteredByName.filter { q =>
          tagsSet.exists(q.tags.contains(_))
        }
      }
    filteredByTag
  }
}

class JoernDefaultArgumentProvider(maxCallDepth: Int)(implicit context: EngineContext) extends DefaultArgumentProvider {

  override def typeSpecificDefaultArg(argTypeFullName: String): Option[Any] = {
    if (argTypeFullName.endsWith("EngineContext")) {
      Some(context.copy(config = EngineConfig(maxCallDepth = maxCallDepth)))
    } else {
      None
    }
  }
}
