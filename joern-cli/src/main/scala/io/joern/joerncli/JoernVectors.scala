package io.joern.joerncli

import io.joern.joerncli.CpgBasedTool.exitIfInvalid
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Method}
import io.shiftleft.semanticcpg.language.*
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.hashing.MurmurHash3

class BagOfPropertiesForNodes extends EmbeddingGenerator[AstNode, (String, String)] {
  override def structureToString(pair: (String, String)): String = pair._1 + ":" + pair._2
  override def extractObjects(cpg: Cpg): Iterator[AstNode]       = cpg.graph.V.collect { case x: AstNode => x }
  override def enumerateSubStructures(obj: AstNode): List[(String, String)] = {
    val relevantFieldTypes = Set(PropertyNames.NAME, PropertyNames.FULL_NAME, PropertyNames.CODE)
    val relevantFields = obj
      .propertiesMap()
      .entrySet()
      .asScala
      .toList
      .filter { e => relevantFieldTypes.contains(e.getKey) }
      .sortBy(_.getKey)
      .map { e =>
        (e.getKey, e.getValue.toString)
      }
    List(("id", obj.id().toString)) ++ relevantFields ++ List(("label", obj.label))
  }

  override def objectToString(node: AstNode): String = node.id().toString
  override def hash(label: String): String           = label

  override def vectorToString(vector: Map[(String, String), Double]): String = {
    ujson.write(vector.keys.map { case (k, v) => (k, ujson.Str(v)) })
  }

}

class BagOfAPISymbolsForMethods extends EmbeddingGenerator[Method, AstNode] {
  override def extractObjects(cpg: Cpg): Iterator[Method]            = cpg.method
  override def enumerateSubStructures(method: Method): List[AstNode] = method.ast.l
  override def structureToString(astNode: AstNode): String           = astNode.code
  override def objectToString(method: Method): String                = method.fullName
}

object EmbeddingGenerator {
  type SparseVectorWithExplicitFeature[S] = Map[String, (Double, S)]
  type SparseVector                       = Map[Int, Double]
}

/** Creates an embedding from a code property graph by following three steps: (1) Objects are extracted from the graph,
  * each of which is ultimately to be mapped to one vector (2) For each object, enumerate its sub structures. (3) Employ
  * feature hashing to associate each sub structure with a dimension. See "Pattern-based Vulnerability Discovery -
  * Chapter 3"
  *
  * T: Object type S: Sub structure type
  */
trait EmbeddingGenerator[T, S] {
  import EmbeddingGenerator.*

  case class Embedding(data: () => Iterator[(T, SparseVectorWithExplicitFeature[S])]) {
    lazy val dimToStructure: Map[String, S] = {
      val m = mutable.HashMap[String, S]()
      data().foreach { case (_, vector) =>
        vector.foreach { case (hash, (_, structure)) =>
          m.put(hash, structure)
        }
      }
      m.toMap
    }

    lazy val structureToDim: Map[S, String] = for ((k, v) <- dimToStructure) yield (v, k)

    def objects: Iterator[String] = data().map { case (obj, _) => objectToString(obj) }

    def vectors: Iterator[Map[S, Double]] = data().map { case (_, vector) =>
      vector.map { case (_, (v, structure)) => structure -> v }
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

  private def vectorize(substructures: List[S]): Map[String, (Double, S)] = {
    substructures
      .groupBy(x => structureToString(x))
      .view
      .map { case (_, l) =>
        val v = l.size
        hash(structureToString(l.head)) -> (v.toDouble, l.head)
      }
      .toMap
  }

  def hash(label: String): String = MurmurHash3.stringHash(label).toString

  def structureToString(s: S): String

  /** A function that creates a sequence of objects from a CPG
    */
  def extractObjects(cpg: Cpg): Iterator[T]

  /** A function that, for a given object, extracts its sub structures
    */
  def enumerateSubStructures(obj: T): List[S]

  def objectToString(t: T): String

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  def vectorToString(vector: Map[S, Double]): String = defaultToString(vector)

  def defaultToString[M](v: M): String = Serialization.write(v)

}

object JoernVectors {

  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  case class Config(cpgFileName: String = "cpg.bin", outDir: String = "out", dimToFeature: Boolean = false)

  def main(args: Array[String]) = {
    parseConfig(args).foreach { config =>
      exitIfInvalid(config.outDir, config.cpgFileName)
      Using.resource(CpgBasedTool.loadFromOdb(config.cpgFileName)) { cpg =>
        val generator = new BagOfPropertiesForNodes()
        val embedding = generator.embed(cpg)
        println("{")
        println("\"objects\":")
        traversalToJson(embedding.objects, generator.defaultToString)
        if (config.dimToFeature) {
          println(",\"dimToFeature\": ")
          println(Serialization.write(embedding.dimToStructure))
        }
        println(",\"vectors\":")
        traversalToJson(embedding.vectors, generator.vectorToString)
        println(",\"edges\":")
        traversalToJson(
          cpg.graph.edges().map { x =>
            Map("src" -> x.outNode().id(), "dst" -> x.inNode().id(), "label" -> x.label())
          },
          generator.defaultToString
        )
        println("}")
      }
    }
  }

  private def parseConfig(args: Array[String]): Option[Config] =
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

  private def traversalToJson[X](trav: Iterator[X], vectorToString: X => String): Unit = {
    println("[")
    trav.nextOption().foreach { vector => print(vectorToString(vector)) }
    trav.foreach { vector => print(",\n" + vectorToString(vector)) }
    println("]")
  }

}
