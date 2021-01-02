package io.shiftleft.joern

import io.shiftleft.console.scan.{ScanPass, outputFindings}
import io.shiftleft.console.{BridgeBase, DefaultArgumentProvider, JoernProduct, Query, QueryDatabase}
import io.shiftleft.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.joern.console.AmmoniteBridge
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import org.json4s.{Formats, NoTypeHints}
import org.json4s.native.Serialization

import scala.reflect.runtime.universe._

case class JoernScanConfig(src: String = "", overwrite: Boolean = false, store: Boolean = false, dump: Boolean = false)

object JoernScan extends App with BridgeBase {

  def parseScanConfig(args: Array[String]): Option[JoernScanConfig] = {
    new scopt.OptionParser[JoernScanConfig]("joern-scan") {
      head("Scan code")
      help("help")
      arg[String]("src")
        .text("source code directory to scan")
        .action((x, c) => c.copy(src = x))

      opt[Unit]("overwrite")
        .action((_, c) => c.copy(overwrite = true))
        .text("Overwrite CPG if it already exists")

      opt[Unit]("store")
        .action((_, c) => c.copy(store = true))
        .text("Store graph changes made by scanner")

      opt[Unit]("dump")
        .action((_, c) => c.copy(dump = true))
        .text("Dump available queries to `querydb.json`")
    }
  }.parse(args, JoernScanConfig())

  if (args.contains("--dump")) {
    dumpQueries()
  } else {
    parseScanConfig(args).foreach { config =>
      val shellConfig = io.shiftleft.console
        .Config()
        .copy(bundleToRun = Some("scan"), src = Some(config.src), overwrite = config.overwrite, store = config.store)
      runAmmonite(shellConfig, JoernProduct)
    }
  }

  private def dumpQueries(): Unit = {
    implicit val engineContext: EngineContext = null
    implicit val formats: AnyRef with Formats = Serialization.formats(NoTypeHints)
    val queryDb = new QueryDatabase(new JoernDefaultArgumentProvider())
    val outFileName = "querydb.json"
    better.files
      .File(outFileName)
      .write(
        Serialization.write(queryDb.allQueries)
      )
    println(s"Queries written to: $outFileName")
  }

  override protected def predefPlus(lines: List[String]): String = AmmoniteBridge.predefPlus(lines)
  override protected def shutdownHooks: List[String] = AmmoniteBridge.shutdownHooks
  override protected def promptStr() = AmmoniteBridge.promptStr()
}

object Scan {
  val overlayName = "scan"
  val description = "Joern Code Scanner"
  def defaultOpts = new ScanOptions()
}

class ScanOptions() extends LayerCreatorOptions {}

class Scan(options: ScanOptions)(implicit engineContext: EngineContext) extends LayerCreator {

  override val overlayName: String = Scan.overlayName
  override val description: String = Scan.description

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val queryDb = new QueryDatabase(new JoernDefaultArgumentProvider())
    val allQueries: List[Query] = queryDb.allQueries
    if (allQueries.isEmpty) {
      println("You have not installed any query bundles")
    }
    runPass(new ScanPass(context.cpg, allQueries), context, storeUndoInfo)
    outputFindings(context.cpg)
  }
}

class JoernDefaultArgumentProvider(implicit context: EngineContext) extends DefaultArgumentProvider {

  override def defaultArgument(method: MethodSymbol, im: InstanceMirror, x: Symbol, i: Int): Option[Any] = {
    if (x.typeSignature.toString.endsWith("EngineContext")) {
      Some(context)
    } else {
      super.defaultArgument(method, im, x, i)
    }
  }
}
