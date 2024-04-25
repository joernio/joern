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
      logger.warn("No JSON files found.")
      InputStream.nullInputStream()
    } else {
      resourcePaths.foreach{ file =>
        val fisshy = classLoader.getResourceAsStream(file)
        val zis = new ZipInputStream(fisshy)
        LazyList.continually(zis.getNextEntry) .takeWhile(_ != null).foreach{ file2 =>
          val fout = new FileOutputStream(file2.getName)
          val buffer = new Array[Byte](1024)
          LazyList.continually(zis.read(buffer)).takeWhile(_ != -1).foreach(fout.write(buffer, 0, _))
        }
      }
      logger.warn("WE FOUND EM")
    }
  }
}


case class RubyMethod(
  name: String,
  parameterTypes: List[(String, String)],
  returnType: String,
  baseTypeFullName: Option[String]
) extends MethodLike

case class RubyField(name: String, typeName: String) extends FieldLike

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
