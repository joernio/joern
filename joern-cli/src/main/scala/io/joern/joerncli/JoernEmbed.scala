package io.joern.joerncli

import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Method}

import scala.util.Using
import io.shiftleft.semanticcpg.language._
import org.json4s.DefaultFormats
import org.json4s.native.Serialization
import overflowdb.traversal._

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

class BagOfAPISymbolsForMethods extends EmbeddingGenerator[Method, AstNode] {
  override def extractObjects(cpg: Cpg): Traversal[Method]           = cpg.method
  override def enumerateSubStructures(method: Method): List[AstNode] = method.ast.l
  override def structureToString(astNode: AstNode): String           = astNode.code
}

/** Creates an embedding from a code property graph by following three steps: (1) Objects are extracted from the graph,
  * each of which is ultimately to be mapped to one vector (2) For each object, enumerate its sub structures. (3) Employ
  * feature hashing to associate each sub structure with a dimension. See "Pattern-based Vulnerability Discovery -
  * Chapter 3"
  */
trait EmbeddingGenerator[T, S] {

  type SparseVector = Map[Int, (Double, String)]

  case class Embedding(data: Traversal[(T, SparseVector)]) {
    lazy val dimToStructure: Map[Int, String] = {
      val m = mutable.HashMap[Int, String]()
      clone(data).foreach { case (_, vector) =>
        vector.foreach { case (hash, (_, structureAsString)) =>
          m.put(hash, structureAsString)
        }
      }
      m.toMap
    }

    lazy val structureToDim: Map[String, Int] = for ((k, v) <- dimToStructure) yield (v, k)

    def vectors: Traversal[Map[Int, Double]] = clone(data).map { case (_, vector) =>
      vector.map { case (dim, (v, _)) => dim -> v }
    }

    private def clone[X](x: Traversal[X]) = Traversal.from(x.iterator.duplicate._2)

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
    )

  }

  private def vectorize(substructures: List[S]): SparseVector = {
    substructures
      .map(structureToString)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .map { case (s, v) =>
        hash(s) -> (v.toDouble, s)
      }
      .toMap
  }

  def hash(label: String): Int = MurmurHash3.stringHash(label)

  def structureToString(s: S): String

  /** A function that creates a sequence of objects from a CPG
    */
  def extractObjects(cpg: Cpg): Traversal[T]

  /** A function that, for a given object, extracts its sub structures
    */
  def enumerateSubStructures(obj: T): List[S]

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
      val embedding = new BagOfAPISymbolsForMethods().embed(cpg)

      val vectors                               = embedding.vectors
      implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
      println("[")
      vectors.nextOption().foreach { vector => print(Serialization.write(vector)) }
      vectors.foreach { vector => println(",\n" + Serialization.write(vector)) }
      println("]")
    }
  }

}
