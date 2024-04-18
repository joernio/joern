package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory
import upickle.core.LinkedHashMap

import scala.annotation.targetName
import upickle.default.{ReadWriter, macroRW, read, writeToByteArray}

import java.io.{ByteArrayInputStream, InputStream}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

type NamespaceToTypeMap = Map[String, Set[RubyType]]

class RubyProgramSummary(
  initialNamespaceMap: NamespaceToTypeMap = Map.empty,
  initialPathMap: Map[String, Set[RubyType]] = Map.empty
) extends ProgramSummary[RubyType] {

  override val namespaceToType: Map[String, Set[RubyType]] = initialNamespaceMap
  val pathToType: Map[String, Set[RubyType]]               = initialPathMap

  @targetName("add")
  def ++(other: RubyProgramSummary): RubyProgramSummary = {
    RubyProgramSummary(
      ProgramSummary.combine(this.namespaceToType, other.namespaceToType),
      ProgramSummary.combine(this.pathToType, other.pathToType)
    )
  }
}

object RubyProgramSummary {
  private val logger = LoggerFactory.getLogger(getClass)

  def BuiltinTypes: NamespaceToTypeMap = {
    messagePackToInitialMapping(mergeBuiltinTypes) match
      case Failure(exception) => logger.warn("Unable to parse JSON type entry from builtin types", exception); Map.empty
      case Success(mapping)   => mapping
  }

  def messagePackToInitialMapping(jsonInputStream: InputStream): Try[NamespaceToTypeMap] =
    Try(read[NamespaceToTypeMap](ujson.Readable.fromByteArray(jsonInputStream.readAllBytes())))


  def mergeBuiltinTypes: InputStream = {
    val classLoader      = getClass.getClassLoader
    val builtinDirectory = "builtin_types"

    /*
      Doing this because java actually cannot read directories from the classPath.
      We're assuming there's no further nesting in the builtin_types directory structure.
     */
    val resourcePaths =
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
        .filter(_.endsWith(".mpk"))


    if (resourcePaths.isEmpty) {
      logger.warn("No MessagePak files found.")
      InputStream.nullInputStream()
    } else {
      val mergedJsonObjects = ListBuffer[LinkedHashMap[String, ujson.Value]]()
      for (resourcePath <- resourcePaths) {
        val inputStream = classLoader.getResourceAsStream(resourcePath)
        val messagePackBytes = inputStream.readAllBytes()

        val rubyType = upickle.default.readBinary[RubyType](messagePackBytes)
//        mergedJsonObjects.addOne(jsonObject)
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

case class RubyMethod(
  name: String,
  parameterTypes: List[(String, String)],
  returnType: String,
  baseTypeFullName: Option[String]
) extends MethodLike
    derives ReadWriter

case class RubyField(name: String, typeName: String) extends FieldLike derives ReadWriter

case class RubyType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends TypeLike[RubyMethod, RubyField] derives ReadWriter {

  @targetName("add")
  override def +(o: TypeLike[RubyMethod, RubyField]): TypeLike[RubyMethod, RubyField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }

  def hasConstructor: Boolean = {
    methods.exists(_.name == XDefines.ConstructorMethodName)
  }

}
