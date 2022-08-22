package io.joern.joerncli

import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, StoredNode}

import scala.util.Using
import io.shiftleft.semanticcpg.language._

import scala.util.hashing.MurmurHash3

/** Creates an embedding from a code property graph by following three steps: (1) Objects are extracted from the graph,
  * each of which is ultimately to be mapped to one vector (2) For each object, enumerate its sub structures. (3) Employ
  * feature hashing to associate each sub structure with a dimension. See "Pattern-based Vulnerability Discovery -
  * Chapter 3"
  */
trait EmbeddingGenerator[T, S] {

  type SparseVector = Map[Int, (Double, S)]

  case class Embedding(vectors: Map[T, SparseVector]) {
    lazy val dimToStructure: Map[Int, S] = vectors.flatMap { case (_, vector) =>
      vector.map { case (hash, (_, structure)) => hash -> structure }
    }
    lazy val structureToDim: Map[S, Int] = vectors.flatMap { case (_, vector) =>
      vector.map { case (hash, (_, structure)) => structure -> hash }
    }
  }

  /** Extract a sequence of (object, vector) pairs from a cpg.
    */
  def embed(cpg: Cpg): Embedding = {
    Embedding(
      extractObjects(cpg)
        .map { obj => (obj, enumerateSubStructures(obj)) }
        .map { case (obj, substructures) =>
          obj -> vectorize(substructures)
        }
        .toMap
    )

  }

  private def vectorize(substructures: List[S]): SparseVector = {
    substructures
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .map { case (s, v) =>
        hash(s) -> (v.toDouble, s)
      }
      .toMap
  }

  /** A function that creates a sequence of objects from a CPG
    */
  def extractObjects(cpg: Cpg): Seq[T]

  /** A function that, for a given object, extracts its sub structures
    */
  def enumerateSubStructures(obj: T): List[S]

  /** A function that allows hashing of a sub structure
    */
  def hash(s: S): Int

}

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
