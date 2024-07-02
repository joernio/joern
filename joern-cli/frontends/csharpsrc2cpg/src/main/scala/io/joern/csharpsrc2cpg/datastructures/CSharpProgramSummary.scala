package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.Constants
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, OverloadableMethod, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory
import upickle.core.LinkedHashMap
import upickle.default.*

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}
import java.net.JarURLConnection
import scala.collection.mutable
import scala.util.Using
import scala.jdk.CollectionConverters.*

type NamespaceToTypeMap = mutable.Map[String, mutable.Set[CSharpType]]

/** A mapping of type stubs of known types within the scope of the analysis.
  *
  * @param namespaceToType
  *   mappings to create the scope from
  * @see
  *   [[CSharpProgramSummary.jsonToInitialMapping]] for generating initial mappings.
  */
case class CSharpProgramSummary(namespaceToType: NamespaceToTypeMap, imports: Set[String])
    extends ProgramSummary[CSharpType, CSharpMethod, CSharpField] {

  def findGlobalTypes: Set[CSharpType] = namespaceToType.getOrElse(Constants.Global, Set.empty).toSet

  @targetName("appendAll")
  def ++=(other: CSharpProgramSummary): CSharpProgramSummary = {
    new CSharpProgramSummary(ProgramSummary.merge(namespaceToType, other.namespaceToType), imports ++ other.imports)
  }

}

object CSharpProgramSummary {

  // Although System is not included by default
  // the types and their methods are exposed through autoboxing of primitives
  def initialImports: Set[String] = Set("", "System")

  def apply(
    namespaceToType: NamespaceToTypeMap = mutable.Map.empty,
    imports: Set[String] = Set.empty
  ): CSharpProgramSummary =
    new CSharpProgramSummary(namespaceToType, imports)

  def apply(summaries: Iterable[CSharpProgramSummary]): CSharpProgramSummary =
    summaries.foldLeft(CSharpProgramSummary())(_ ++= _)

  private val logger = LoggerFactory.getLogger(getClass)

  /** @return
    *   a mapping of the `System` package types.
    */
  def BuiltinTypes: NamespaceToTypeMap = {
    jsonToInitialMapping(mergeBuiltInTypesJson) match {
      case Failure(exception) =>
        logger.warn("Unable to parse JSON type entry from builtin types", exception); mutable.Map.empty
      case Success(mapping) => mapping
    }
  }

  /** Converts a JSON type mapping to a NamespaceToTypeMap entry.
    * @param jsonInputStream
    *   a JSON file as an input stream.
    * @return
    *   the resulting type map in a Try
    */
  def jsonToInitialMapping(jsonInputStream: InputStream): Try[NamespaceToTypeMap] =
    Try(read[NamespaceToTypeMap](ujson.Readable.fromByteArray(jsonInputStream.readAllBytes())))

  private def mergeBuiltInTypesJson: InputStream = {
    val classLoader      = getClass.getClassLoader
    val builtinDirectory = "builtin_types"

    /*
      Doing this because java actually cannot read directories from the classPath.
      We're assuming there's no further nesting in the builtin_types directory structure.
      TODO: Once MessagePack types and compression is implemented for CSharp, the `resourcePaths` building can
       be moved into `ProgramSummary` since all subclasses of it will need to do this to find builtin types
     */
    val resourcePaths: List[String] = Option(getClass.getClassLoader.getResource(builtinDirectory)) match {
      case Some(url) if url.getProtocol == "jar" =>
        val connection = url.openConnection.asInstanceOf[JarURLConnection]
        Using.resource(connection.getJarFile) { jarFile =>
          jarFile
            .entries()
            .asScala
            .toList
            .map(_.getName)
            .filter(_.startsWith(builtinDirectory))
            .filter(!_.equals(builtinDirectory))
            .filter(_.endsWith(".json"))
        }
      case _ =>
        Source
          .fromResource(builtinDirectory)
          .getLines()
          .toList
          .flatMap(u => {
            val basePath = s"$builtinDirectory/$u"
            Source
              .fromResource(basePath)
              .getLines()
              .toList
              .map(p => {
                s"$basePath/$p"
              })
          })
    }
    if (resourcePaths.isEmpty) {
      logger.warn("No JSON files found.")
      InputStream.nullInputStream()
    } else {
      val mergedJsonObjects = ListBuffer[LinkedHashMap[String, ujson.Value]]()
      for (resourcePath <- resourcePaths) {
        val inputStream = classLoader.getResourceAsStream(resourcePath)
        val jsonString  = Source.fromInputStream(inputStream).mkString
        val jsonObject  = ujson.read(jsonString).obj
        mergedJsonObjects.addOne(jsonObject)
      }

      val mergedJson: LinkedHashMap[String, ujson.Value] =
        mergedJsonObjects
          .reduceOption((prev, curr) => {
            curr.keys.foreach(key => {
              prev.updateWith(key) {
                case Some(x) =>
                  Option(x.arr.addAll(curr.get(key).get.arr))
                case None =>
                  Option(curr.get(key).get.arr)
              }
            })
            prev
          })
          .getOrElse(LinkedHashMap[String, ujson.Value]())

      new ByteArrayInputStream(writeToByteArray(ujson.read(mergedJson)))
    }
  }

}

case class CSharpField(name: String, typeName: String) extends FieldLike derives ReadWriter

case class CSharpMethod(name: String, returnType: String, parameterTypes: List[(String, String)], isStatic: Boolean)
    extends MethodLike
    with OverloadableMethod derives ReadWriter

case class CSharpType(name: String, methods: List[CSharpMethod], fields: List[CSharpField])
    extends TypeLike[CSharpMethod, CSharpField] derives ReadWriter {
  @targetName("add")
  override def +(o: TypeLike[CSharpMethod, CSharpField]): TypeLike[CSharpMethod, CSharpField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }

}
