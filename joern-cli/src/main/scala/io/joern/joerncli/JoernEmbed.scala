package io.joern.joerncli

import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.shiftleft.codepropertygraph.generated.nodes.Method

import scala.util.Using
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import scala.util.hashing.MurmurHash3

object JoernEmbed extends App {

  case class Config(cpgFileName: String = "cpg.bin", outDir: String = "out")

  private def parseConfig: Option[Config] =
    new scopt.OptionParser[Config]("joern-embed") {
      head("Extract vector representations of code from CPG")
      help("help")
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]('o', "out")
        .text("output directory - will be created and must not yet exist")
        .action((x, c) => c.copy(outDir = x))
    }.parse(args, Config())

  parseConfig.foreach { config =>
    exitIfInvalid(config.outDir, config.cpgFileName)
    Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
      println(cpg.method.map { m =>
        val vector = toVector(m)
        (m.fullName, vector)
      }.toJsonPretty)
    }
  }

  private def toVector(method: Method): Map[Int, Int] = {
    method.ast.code.l
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .map { case (k, v) =>
        MurmurHash3.stringHash(k) -> v
      }
      .toMap
  }

}
