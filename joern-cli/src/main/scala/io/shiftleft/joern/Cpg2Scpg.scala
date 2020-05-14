package io.shiftleft.joern

import io.shiftleft.dataflowengine.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.dataflowengine.semanticsloader.SemanticsLoader
import io.shiftleft.semanticcpg.layers.{LayerCreatorContext, Scpg}
import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

object Cpg2Scpg extends App {

  val DEFAULT_CPG_IN_FILE = "cpg.bin"

  private val logger = LoggerFactory.getLogger(getClass)

  parseConfig.foreach { config =>
    try {
      val cpg = run(config.inputPath, config.dataFlow, config.semanticsFile)
      logger.info(s"Closing Cpg...")
      val startTime = System.currentTimeMillis()
      cpg.close()
      logger.info(s"Done in : ${System.currentTimeMillis() - startTime} ms")

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
    * Load the CPG at `storeFilename` and add enhancements,
    * turning the CPG into an SCPG.
    * @param storeFilename the filename of the cpg
    * */
  def run(storeFilename: String, dataFlow: Boolean, semanticsFilename: String): Cpg = {
    val cpg = CpgLoader.loadFromOdb(storeFilename)
    val context = new LayerCreatorContext(cpg, new SerializedCpg())
    new Scpg().run(context)
    if (dataFlow) {
      val options = new OssDataFlowOptions(semanticsFilename)
      new OssDataFlow(() => options).run(context)
    }
    cpg
  }

}
