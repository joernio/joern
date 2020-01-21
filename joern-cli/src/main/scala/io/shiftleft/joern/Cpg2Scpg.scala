package io.shiftleft.joern

import better.files.File
import io.shiftleft.dataflowengine.layers.dataflows.DataFlowRunner
import io.shiftleft.dataflowengine.semanticsloader.SemanticsLoader
import io.shiftleft.semanticcpg.layers.EnhancementRunner
import io.shiftleft.SerializedCpg
import org.slf4j.LoggerFactory

object Cpg2Scpg extends App {

  val DEFAULT_CPG_IN_FILE = "cpg.bin"

  private val logger = LoggerFactory.getLogger(getClass)

  parseConfig.foreach { config =>
    try {
      run(config.inputPath, "store.bin", config.dataFlow, config.semanticsFile)
    } catch {
      case exception: Exception =>
        logger.error("Failed to generate CPG.", exception)
        System.exit(1)
    }
    System.exit(0)
  }

  case class Config(inputPath: String, dataFlow: Boolean, semanticsFile: String)
  def parseConfig: Option[Config] =
    new scopt.OptionParser[Config](getClass.getSimpleName) {
      arg[String]("<cpg>")
        .text("CPG to enhance")
        .action((x, c) => c.copy(inputPath = x))
      opt[Unit]("nodataflow")
        .text("do not perform data flow analysis")
        .action((x, c) => c.copy(dataFlow = false))
      opt[String]("semanticsfile")
        .text("data flow semantics file")
        .action((x, c) => c.copy(semanticsFile = x))

    }.parse(args, Config(DEFAULT_CPG_IN_FILE, true, CpgLoader.defaultSemanticsFile))

  /**
    * Load the CPG at `cpgFilename` and add enhancements,
    * turning the CPG into an SCPG.
    * @param storeFilename the filename of the cpg
    * */
  def run(inputFilename: String, storeFilename: String, dataFlow: Boolean, semanticsFilename: String): Unit = {
    val storeFile = File(storeFilename)
    if (storeFilename != "" && storeFile.exists) { storeFile.delete() }
    val cpg = CpgLoader.load(inputFilename, storeFilename)
    new EnhancementRunner().run(cpg, new SerializedCpg())
    if (dataFlow) {
      val semantics = new SemanticsLoader(semanticsFilename).load()
      new DataFlowRunner(semantics).run(cpg, new SerializedCpg())
    }
    cpg.graph.close()
  }

}
