package io.joern.joerncli

import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Method}

import scala.util.Using
import io.shiftleft.semanticcpg.language._
import org.json4s.DefaultFormats
import org.json4s.native.Serialization
import overflowdb.traversal._
import scala.jdk.CollectionConverters._

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

class BagOfPropertiesForNodes extends EmbeddingGenerator[AstNode, String] {
  override def structureToString(s: String): String         = s
  override def extractObjects(cpg: Cpg): Traversal[AstNode] = cpg.method.ast
  override def enumerateSubStructures(obj: AstNode): List[String] =
    List(obj.id().toString) ++ obj.propertiesMap().entrySet().asScala.toList.sortBy(_.getKey).map { e =>
      s"${e.getKey}: ${e.getValue}"
    }
  override def objectToString(node: AstNode): String = node.id().toString
  override def hash(label: String): String           = label
}

class BagOfAPISymbolsForMethods extends EmbeddingGenerator[Method, AstNode] {
  override def extractObjects(cpg: Cpg): Traversal[Method]           = cpg.method
  override def enumerateSubStructures(method: Method): List[AstNode] = method.ast.l
  override def structureToString(astNode: AstNode): String           = astNode.code
  override def objectToString(method: Method): String                = method.fullName
}

object EmbeddingGenerator {
  type SparseVectorWithExplicitFeature = Map[String, (Double, String)]
  type SparseVector                    = Map[Int, Double]
}

/** Creates an embedding from a code property graph by following three steps: (1) Objects are extracted from the graph,
  * each of which is ultimately to be mapped to one vector (2) For each object, enumerate its sub structures. (3) Employ
  * feature hashing to associate each sub structure with a dimension. See "Pattern-based Vulnerability Discovery -
  * Chapter 3"
  */
trait EmbeddingGenerator[T, S] {
  import EmbeddingGenerator._

  case class Embedding(data: (() => Traversal[(T, SparseVectorWithExplicitFeature)])) {
    lazy val dimToStructure: Map[String, String] = {
      val m = mutable.HashMap[String, String]()
      data().foreach { case (_, vector) =>
        vector.foreach { case (hash, (_, structureAsString)) =>
          m.put(hash, structureAsString)
        }
      }
      m.toMap
    }

    lazy val structureToDim: Map[String, String] = for ((k, v) <- dimToStructure) yield (v, k)

    def objects: Traversal[String] = data().map { case (obj, _) => objectToString(obj) }

    def vectors: Traversal[Map[String, Double]] = data().map { case (_, vector) =>
      vector.map { case (dim, (v, _)) => dim -> v }
    }

  }

  /** Extract a sequence of (object, vector) pairs from a cpg.
    */
  def embed(cpg: Cpg): Embedding = {
    Embedding({ () =>
      extractObjects(cpg)
        .map { obj =>
          val substructures = enumerateSubStructures(obj)
          obj -> vectorize(substructures)
        }
    })
  }

  private def vectorize(substructures: List[S]): SparseVectorWithExplicitFeature = {
    substructures
      .map(x => structureToString(x))
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .map { case (s, v) =>
        hash(s) -> (v.toDouble, s)
      }
      .toMap
  }

  def hash(label: String): String = MurmurHash3.stringHash(label).toString

  def structureToString(s: S): String

  /** A function that creates a sequence of objects from a CPG
    */
  def extractObjects(cpg: Cpg): Traversal[T]

  /** A function that, for a given object, extracts its sub structures
    */
  def enumerateSubStructures(obj: T): List[S]

  def objectToString(t: T): String

}

object JoernVectors extends App {

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  case class Config(cpgFileName: String = "cpg.bin", outDir: String = "out", dimToFeature: Boolean = false)

  private def parseConfig: Option[Config] =
    new scopt.OptionParser[Config]("joern-vectors") {
      head("Extract vector representations of code from CPG")
      help("help")
      arg[String]("cpg")
        .text("input CPG file name - defaults to `cpg.bin`")
        .optional()
        .action((x, c) => c.copy(cpgFileName = x))
      opt[String]('o', "out")
        .text("output directory - will be created and must not yet exist")
        .action((x, c) => c.copy(outDir = x))
      opt[Unit]("features")
        .text("Provide map from dimensions to features")
        .action((_, c) => c.copy(dimToFeature = true))
    }.parse(args, Config())

  parseConfig.foreach { config =>
    exitIfInvalid(config.outDir, config.cpgFileName)
    Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
      val embedding = new BagOfPropertiesForNodes().embed(cpg)
      println("{")
      println("\"objects\":")
      traversalToJson(embedding.objects)
      if (config.dimToFeature) {
        println(",\"dimToFeature\": ")
        println(Serialization.write(embedding.dimToStructure))
      }
      println(",\"vectors\":")
      traversalToJson(embedding.vectors)
      println(",\"edges\":")
      traversalToJson(cpg.graph.edges().map { x =>
        Map("src" -> x.outNode().id(), "dst" -> x.inNode().id(), "label" -> x.label())
      })
      println("}")
    }
  }

  private def traversalToJson[X](trav: Traversal[X]): Unit = {
    println("[")
    trav.nextOption().foreach { vector => print(Serialization.write(vector)) }
    trav.foreach { vector => print(",\n" + Serialization.write(vector)) }
    println("]")
  }

}
