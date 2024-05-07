package io.joern.rubysrc2cpg.datastructures

import better.files.File
import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, StubbedType, TypeLike}
import io.joern.x2cpg.typestub.{TypeStubMetaData, TypeStubUtil}
import org.slf4j.LoggerFactory

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.targetName
import scala.io.Source
import java.net.JarURLConnection
import java.util.zip.ZipInputStream
import scala.util.{Failure, Success, Try, Using}
import scala.jdk.CollectionConverters.*
import upickle.default.*

import scala.collection.mutable.ListBuffer

type NamespaceToTypeMap = Map[String, Set[RubyType]]

class RubyProgramSummary(
  initialNamespaceMap: NamespaceToTypeMap = Map.empty,
  initialPathMap: NamespaceToTypeMap = Map.empty
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

  def BuiltinTypes(implicit typeStubMetaData: TypeStubMetaData): NamespaceToTypeMap = {
    if (typeStubMetaData.useTypeStubs) {
      mpkZipToInitialMapping(mergeBuiltinMpkZip) match {
        case Failure(exception) => logger.warn("Unable to parse builtin types", exception); Map.empty
        case Success(mapping)   => mapping
      }
    } else {
      Map.empty
    }
  }

  private def mpkZipToInitialMapping(inputStream: InputStream): Try[NamespaceToTypeMap] = {
    Try(readBinary[NamespaceToTypeMap](inputStream.readAllBytes()))
  }

  private def mergeBuiltinMpkZip(implicit typeStubMetaData: TypeStubMetaData): InputStream = {
    val classLoader = getClass.getClassLoader
    val typeStubDir = TypeStubUtil.typeStubDir

    val typeStubFiles: Seq[File] =
      typeStubDir
        .walk()
        .filter(f => f.isRegularFile && f.name.startsWith("rubysrc") && f.`extension`.contains(".zip"))
        .toSeq

    if (typeStubFiles.isEmpty) {
      logger.warn("No ZIP files found.")
      InputStream.nullInputStream()
    } else {
      val mergedMpksObj = ListBuffer[collection.mutable.Map[String, Set[RubyStubbedType]]]()
      typeStubFiles.foreach { f =>
        f.fileInputStream { fis =>
          val zis = new ZipInputStream(fis)

          LazyList.continually(zis.getNextEntry).takeWhile(_ != null).foreach { file =>
            val mpkObj =
              upickle.default.readBinary[collection.mutable.Map[String, Set[RubyStubbedType]]](zis.readAllBytes())
            mergedMpksObj.addOne(mpkObj)
          }
        }
      }

      val mergedMpks = mergedMpksObj
        .reduceOption((prev, curr) => {
          curr.keys.foreach(key => {
            prev.updateWith(key) {
              case Some(x) =>
                Option(x ++ curr(key))
              case None =>
                Option(curr(key))
            }
          })
          prev
        })
        .getOrElse(collection.mutable.Map[String, Set[RubyStubbedType]]())

      new ByteArrayInputStream(upickle.default.writeBinary(mergedMpks))
    }
  }
}

case class RubyMethod(
  name: String,
  parameterTypes: List[(String, String)],
  returnType: String,
  baseTypeFullName: Option[String]
) extends MethodLike

object RubyMethod {
  implicit val rubyMethodRwJson: ReadWriter[RubyMethod] = readwriter[ujson.Value].bimap[RubyMethod](
    x => ujson.Obj("name" -> x.name),
    json =>
      RubyMethod(
        name = json("name").str,
        parameterTypes = List.empty,
        returnType = XDefines.Any,
        baseTypeFullName = Option(json("name").str.split("\\.").dropRight(1).mkString("."))
      )
  )
}

case class RubyField(name: String, typeName: String) extends FieldLike derives ReadWriter

class RubyStubbedType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends RubyType(name, methods, fields)
    with StubbedType[RubyMethod, RubyField]

object RubyStubbedType {
  implicit val rubyTypeRw: ReadWriter[RubyStubbedType] = readwriter[ujson.Value].bimap[RubyStubbedType](
    x =>
      ujson.Obj(
        "name" -> x.name,
        "methods" -> x.methods.map { method =>
          ujson.Obj("name" -> method.name)
        },
        "fields" -> x.fields.map { field => write[RubyField](field) }
      ),
    json =>
      RubyStubbedType(
        name = json("name").str,
        methods = json.obj.get("methods") match {
          case Some(jsonMethods) =>
            val methodsList = read[List[RubyMethod]](jsonMethods)

            methodsList.map { func =>
              val baseTypeFullName = json("name").str

              func.copy(name = func.name, baseTypeFullName = Option(baseTypeFullName))
            }
          case None => Nil
        },
        fields = json.obj.get("fields").map(read[List[RubyField]](_)).getOrElse(Nil)
      )
  )
}

case class RubyType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends TypeLike[RubyMethod, RubyField] {

  @targetName("add")
  override def +(o: TypeLike[RubyMethod, RubyField]): TypeLike[RubyMethod, RubyField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }

  def hasConstructor: Boolean = {
    methods.exists(_.name == XDefines.ConstructorMethodName)
  }
}

object RubyType {
  implicit val rubyTypeRw: ReadWriter[RubyType] = readwriter[ujson.Value].bimap[RubyType](
    x =>
      ujson.Obj(
        "name" -> x.name,
        "methods" -> x.methods.map { method =>
          ujson.Obj("name" -> method.name)
        },
        "fields" -> x.fields.map { field => write[RubyField](field) }
      ),
    json =>
      RubyType(
        name = json("name").str,
        methods = json.obj.get("methods") match {
          case Some(jsonMethods) =>
            val methodsList = read[List[RubyMethod]](jsonMethods)

            methodsList.map { func =>
              val splitName        = func.name.split("\\.")
              val baseTypeFullName = splitName.dropRight(1).mkString(".")

              func.copy(name = func.name, baseTypeFullName = Option(baseTypeFullName))
            }
          case None => Nil
        },
        fields = json.obj.get("fields").map(read[List[RubyField]](_)).getOrElse(Nil)
      )
  )
}
