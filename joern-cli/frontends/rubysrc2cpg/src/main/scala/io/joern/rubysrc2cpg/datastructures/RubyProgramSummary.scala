package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory

import java.io.InputStream
import scala.annotation.targetName
import scala.io.Source

import java.net.JarURLConnection
import scala.util.Using

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

    val foile = Source.fromFile("src/main/resources/builtin_types")
    println(foile)


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
