package io.joern.joerncli

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.language.*

case class FlowConfig(
  cpgFileName: String = "cpg.bin",
  verbose: Boolean = false,
  srcRegex: String = ".*",
  dstRegex: String = ".*",
  srcParam: Option[Int] = None,
  dstParam: Option[Int] = None,
  depth: Int = 1
)

object JoernFlow {
  def main(args: Array[String]) = {
    parseConfig(args).foreach { config =>
      def debugOut(msg: String): Unit = {
        if (config.verbose) {
          print(msg)
        }
      }

      debugOut("Loading graph... ")
      val cpg = CpgBasedTool.loadFromFile(config.cpgFileName)
      debugOut("[DONE]\n")

      implicit val resolver: ICallResolver = NoResolve
      val sources                          = params(cpg, config.srcRegex, config.srcParam)
      val sinks                            = params(cpg, config.dstRegex, config.dstParam)

      debugOut(s"Number of sources: ${sources.size}\n")
      debugOut(s"Number of sinks: ${sinks.size}\n")

      implicit val semantics: Semantics = DefaultSemantics()
      val engineConfig                  = EngineConfig(config.depth)
      debugOut(s"Analysis depth: ${engineConfig.maxCallDepth}\n")
      implicit val context: EngineContext = EngineContext(semantics, engineConfig)

      debugOut("Determining flows...")
      sinks.foreach { s =>
        List(s).reachableByFlows(sources.iterator).p.foreach(println)
      }
      debugOut("[DONE]")

      debugOut("Closing graph... ")
      cpg.close()
      debugOut("[DONE]\n")
    }
  }

  private def parseConfig(args: Array[String]): Option[FlowConfig] = {
    new scopt.OptionParser[FlowConfig]("joern-flow") {
      head("Find flows")
      help("help")

      arg[String]("src")
        .text("source regex")
        .action((x, c) => c.copy(srcRegex = x))

      arg[String]("dst")
        .text("destination regex")
        .action((x, c) => c.copy(dstRegex = x))

      arg[String]("cpg")
        .text("CPG file name ('cpg.bin' by default)")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))

      opt[Int]("src-param")
        .text("Source parameter")
        .optional()
        .action((x, c) => c.copy(dstParam = Some(x)))

      opt[Int]("dst-param")
        .text("Destination parameter")
        .optional()
        .action((x, c) => c.copy(dstParam = Some(x)))

      opt[Int]("depth")
        .text("Analysis depth (number of calls to expand)")
        .optional()
        .action((x, c) => c.copy(depth = x))

      opt[Unit]("verbose")
        .text("Print debug information")
        .optional()
        .action((_, c) => c.copy(verbose = true))
    }
  }.parse(args, FlowConfig())

  private def params(cpg: Cpg, methodNameRegex: String, paramIndex: Option[Int]): List[MethodParameterIn] = {
    cpg
      .method(methodNameRegex)
      .parameter
      .filter { p =>
        paramIndex.isEmpty || paramIndex.contains(p.order)
      }
      .l
  }

}
