package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory

import java.io.{FileInputStream, FileOutputStream, InputStream}
import scala.annotation.targetName
import scala.io.Source
import java.net.JarURLConnection
import java.util.zip.ZipInputStream
import scala.util.Using
import scala.jdk.CollectionConverters.*
import upickle.default.*

import java.nio.charset.StandardCharsets

class RubyProgramSummary(
  initialNamespaceMap: Map[String, Set[RubyType]] = Map.empty,
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

  def readme(): Unit = {
    val classLoader = getClass.getClassLoader
    val builtinDirectory = "builtin_types"

    val resourcePaths: List[String] =
      Option(getClass.getClassLoader.getResource(builtinDirectory)) match {
        case Some(url) if url.getProtocol == "jar" =>
          val connection = url.openConnection.asInstanceOf[JarURLConnection]
          Using.resource(connection.getJarFile) { jarFile =>
            jarFile.entries().asScala
              .toList
              .map(_.getName)
              .filter(_.startsWith(builtinDirectory))
              .filter(!_.equals(builtinDirectory))
              .filter(_.endsWith(".zip"))
          }
        case _ =>
          Source
            .fromResource(builtinDirectory)
            .getLines()
            .toList
            .map(u => {
              val basePath = s"$builtinDirectory/$u"
              basePath
            })
            .filter(_.endsWith(".zip"))
      }
    if (resourcePaths.isEmpty) {
      logger.warn("No ZIP files found.")
      InputStream.nullInputStream()
    } else {
      resourcePaths.foreach{ path =>
        val fis = classLoader.getResourceAsStream(path)
        val zis = new ZipInputStream(fis)

        LazyList.continually(zis.getNextEntry).takeWhile(_ != null).foreach{ file =>
          val method = upickle.default.readBinary[collection.mutable.Map[String, List[RubyType]]](zis.readAllBytes())
        }
      }
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
            }.toList
          case None => Nil
        },
        fields = json.obj.get("fields").map(read[List[RubyField]](_)).getOrElse(Nil)
      )
  )
}
