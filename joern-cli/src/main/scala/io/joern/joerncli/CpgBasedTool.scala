package io.joern.joerncli

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.language.*

object CpgBasedTool {

  /** Load code property graph from overflowDB
    *
    * @param filename
    *   name of the file that stores the CPG
    */
  def loadFromOdb(filename: String): Cpg = {
    val odbConfig = overflowdb.Config.withDefaults().withStorageLocation(filename)
    val config    = CpgLoaderConfig().withOverflowConfig(odbConfig).doNotCreateIndexesOnLoad
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.loadFromOverflowDb(config)
  }

  /** Add the data flow layer to the CPG if it does not exist yet.
    */
  def addDataFlowOverlayIfNonExistent(cpg: Cpg)(implicit s: Semantics): Unit = {
    if (!cpg.metaData.overlays.exists(_ == OssDataFlow.overlayName)) {
      System.err.println("CPG does not have dataflow overlay. Calculating.")
      val opts    = new OssDataFlowOptions()
      val context = new LayerCreatorContext(cpg)
      new OssDataFlow(opts).run(context)
    }
  }

  /** Create an informational string for the user that informs of a successfully generated CPG.
    */
  def newCpgCreatedString(path: String): String = {
    val absolutePath = File(path).path.toAbsolutePath
    s"Successfully wrote graph to: $absolutePath\n" +
      s"To load the graph, type `joern $absolutePath`"
  }

  val ARGS_DELIMITER = "--frontend-args"

  /** Splits arguments at the ARGS_DELIMITER into arguments for the tool and arguments for the language frontend.
    */
  def splitArgs(args: Array[String]): (List[String], List[String]) = {
    args.indexOf(ARGS_DELIMITER) match {
      case -1 => (args.toList, Nil)
      case splitIdx =>
        val (parseOpts, frontendOpts) = args.toList.splitAt(splitIdx)
        (parseOpts, frontendOpts.tail) // Take the tail to ignore the delimiter
    }
  }

  def exitIfInvalid(outDir: String, cpgFileName: String): Unit = {
    if (File(outDir).exists)
      exitWithError(s"Output directory `$outDir` already exists.")
    if (File(cpgFileName).notExists)
      exitWithError(s"CPG at $cpgFileName does not exist.")
  }

  def exitWithError(msg: String): Unit = {
    System.err.println(s"error: $msg")
    System.exit(1)
  }

}
