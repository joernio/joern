package io.shiftleft.joern
import io.shiftleft.semanticcpg.language._
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.joern.console.JoernWorkspaceLoader
import overflowdb.traversal.Traversal

case class FlowConfig(cpgFileName: String = "cpg.bin",
                      srcRegex: String = ".*",
                      dstRegex: String = ".*",
                      srcParam: Option[Int] = None,
                      dstParam: Option[Int] = None)

object JoernFlow extends App {

  private def parseConfig: Option[FlowConfig] = {
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
        .action((x, c) => c.copy(srcRegex = x))

      opt[Int]("src-param")
        .text("Source parameter")
        .optional()
        .action((x, c) => c.copy(dstParam = Some(x)))

      opt[Int]("dst-param")
        .text("Destination parameter")
        .optional()
        .action((x, c) => c.copy(dstParam = Some(x)))

    }
  }.parse(args, FlowConfig())

  parseConfig.foreach { config =>
    print("Loading graph... ")
    val cpg = CpgBasedTool.loadFromOdb(config.cpgFileName)
    println("[DONE]")

    implicit val resolver: ICallResolver = NoResolve

    val source = cpg.method(config.srcRegex).parameter.filter { p =>
      config.srcParam.isEmpty || config.srcParam.contains(p.order)
    }.l

    val sink = cpg.method(config.dstRegex).parameter.filter { p =>
      config.dstParam.isEmpty || config.dstParam.contains(p.order)
    }.argument.l

    println(s"Sources: ${source.size}")
    println(s"Sinks: ${sink.size}")

    implicit val semantics: Semantics = JoernWorkspaceLoader.defaultSemantics
    val engineConfig = EngineConfig(0)
    println(s"Analysis depth: ${engineConfig.maxCallDepth}")
    implicit val context: EngineContext = EngineContext(semantics, engineConfig)

    println("Determining flows...")
    sink.foreach{s =>
      println("Inspecting: " + List(s).to(Traversal).location.head)
      List(s).to(Traversal).reachableByFlows(source.to(Traversal)).p.foreach(println)
    }
    println("[DONE]")

    print("Closing graph... ")
    cpg.close()
    println("[DONE]")
  }

}
