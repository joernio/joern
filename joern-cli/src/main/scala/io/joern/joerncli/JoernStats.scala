package io.joern.joerncli

import io.shiftleft.semanticcpg.language._

case class StatsConfig(cpgFileName: String = "cpg.bin")

object JoernStats extends App {
  private def parseConfig: Option[StatsConfig] = {
    new scopt.OptionParser[StatsConfig]("joern-stats") {
      head("Prints statistics on various nodes found in Code Property Graphs")
      help("help")

      arg[String]("cpg.bin")
        .text("Path to the Code Property Graph file")
        .action((name, c) => c.copy(cpgFileName = name))
    }
  }.parse(args, StatsConfig())

  parseConfig.foreach { config =>
    val cpg = CpgBasedTool.loadFromOdb(config.cpgFileName)
    if (cpg.metaData.isEmpty) {
      println("ERROR: could not load Code Property Graph file at path `" + config.cpgFileName + "`. Exiting.")
      sys.exit(1)
    }

    println("NAMESPACE " + cpg.namespace.size)
    println("TYPE_DECL " + cpg.typeDecl.size)
    println("METHOD    " + cpg.method.size)
    println("CALL      " + cpg.call.size)
    cpg.close()
  }
}
