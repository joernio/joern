package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{NodeTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.language._
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.joern.console.JoernWorkspaceLoader
import overflowdb.traversal.{Traversal, jIteratortoTraversal}

case class StatsConfig(cpgFileName: String = "cpg.bin")

object JoernStats extends App {
  private def parseConfig: Option[StatsConfig] = {
    new scopt.OptionParser[StatsConfig]("joern-stats") {
      head("Prints statistics on various nodes found in Code Property Graphs")
      help("help")

      arg[String]("cpg")
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

    println("METADATA  " + cpg.metaData.size)
    println("NAMESPACE " + cpg.namespace.size)
    println("TYPE_DECL " + cpg.typeDecl.size)
    println("METHOD    " + cpg.method.size)
    println("CALL      " + cpg.call.size)
    println("FINDING   " + cpg.graph.nodes(NodeTypes.FINDING).size)
    cpg.close()
  }
}
