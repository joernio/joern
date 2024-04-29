package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.Constants
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory
import upickle.core.LinkedHashMap
import upickle.default.*

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
import java.net.JarURLConnection
import scala.util.Using

type NamespaceToTypeMap = Map[String, Set[CSharpType]]

/** A mapping of type stubs of known types within the scope of the analysis.
  *
  * @param initialMappings
  *   mappings to create the scope from
  * @see
  *   [[CSharpProgramSummary.jsonToInitialMapping]] for generating initial mappings.
  */
class CSharpProgramSummary(initialMappings: List[NamespaceToTypeMap] = List.empty) extends ProgramSummary[CSharpType] {

  override val namespaceToType: NamespaceToTypeMap = initialMappings.reduceOption(_ ++ _).getOrElse(Map.empty)
  def findGlobalTypes: Set[CSharpType]             = namespaceToType.getOrElse(Constants.Global, Set.empty)

  @targetName("add")
  def ++(other: CSharpProgramSummary): CSharpProgramSummary = {
    CSharpProgramSummary(ProgramSummary.combine(this.namespaceToType, other.namespaceToType) :: Nil)
  }

}

object CSharpProgramSummary {

  private val logger = LoggerFactory.getLogger(getClass)

  /** @return
    *   a mapping of the `System` package types.
    */
  def BuiltinTypes: NamespaceToTypeMap = {
    jsonToInitialMapping(mergeBuiltInTypesJson) match
      case Failure(exception) => logger.warn("Unable to parse JSON type entry from builtin types", exception); Map.empty
      case Success(mapping)   => mapping
  }

  /** Converts a JSON type mapping to a NamespaceToTypeMap entry.
    * @param jsonInputStream
    *   a JSON file as an input stream.
    * @return
    *   the resulting type map in a Try
    */
  def jsonToInitialMapping(jsonInputStream: InputStream): Try[NamespaceToTypeMap] =
    Try(read[NamespaceToTypeMap](ujson.Readable.fromByteArray(jsonInputStream.readAllBytes())))

  def mergeBuiltInTypesJson: InputStream = {
    val classLoader      = getClass.getClassLoader
    val builtinDirectory = "builtin_types"

    /*
      Doing this because java actually cannot read directories from the classPath.
      We're assuming there's no further nesting in the builtin_types directory structure.
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
    extends MethodLike derives ReadWriter

case class CSharpType(name: String, methods: List[CSharpMethod], fields: List[CSharpField])
    extends TypeLike[CSharpMethod, CSharpField] derives ReadWriter {
  @targetName("add")
  override def +(o: TypeLike[CSharpMethod, CSharpField]): TypeLike[CSharpMethod, CSharpField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }

}
