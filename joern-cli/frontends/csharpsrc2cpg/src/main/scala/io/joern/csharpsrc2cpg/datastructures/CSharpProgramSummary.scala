package io.joern.csharpsrc2cpg.datastructures

import better.files.File.VisitOptions
import io.joern.csharpsrc2cpg.Constants
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, OverloadableMethod, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory
import upickle.core.LinkedHashMap
import upickle.default.*

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.targetName
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
case class CSharpProgramSummary(namespaceToType: NamespaceToTypeMap, imports: Set[String], globalImports: Set[String])
    extends ProgramSummary[CSharpType, CSharpMethod, CSharpField] {

  def findGlobalTypes: Set[CSharpType] = namespaceToType.getOrElse(Constants.Global, Set.empty).toSet

  @targetName("appendAll")
  def ++=(other: CSharpProgramSummary): CSharpProgramSummary = {
    new CSharpProgramSummary(
      ProgramSummary.merge(namespaceToType, other.namespaceToType),
      imports ++ other.imports,
      globalImports ++ other.globalImports
    )
  }

  private def allImports: Set[String] = imports ++ globalImports

  def appendImported(other: CSharpProgramSummary): CSharpProgramSummary =
    this ++= other.filter(namespacePred = (ns, _) => allImports.contains(ns))

  /** Builds a new `CSharpProgramSummary` by filtering the current one's fields.
    *
    * @param namespacePred
    *   filtering predicate for `namespaceToType`
    *
    * @param importsPred
    *   filtering predicate for `imports`
    *
    * @param globalImportsPred
    *   filtering predicate for `globalImports`
    */
  def filter(
    namespacePred: (String, mutable.Set[CSharpType]) => Boolean = (_, _) => true,
    importsPred: String => Boolean = _ => true,
    globalImportsPred: String => Boolean = _ => true
  ): CSharpProgramSummary =
    copy(
      namespaceToType = mutable.Map.fromSpecific(namespaceToType.view.filter(namespacePred(_, _))),
      imports = imports.filter(importsPred),
      globalImports = globalImports.filter(globalImportsPred)
    )

  def addGlobalImports(imports: Set[String]): CSharpProgramSummary =
    copy(globalImports = globalImports ++ imports)

}

object CSharpProgramSummary {

  // Although System is not included by default
  // the types and their methods are exposed through autoboxing of primitives
  def initialImports: Set[String] = Set("", "System")

  def apply(
    namespaceToType: NamespaceToTypeMap = mutable.Map.empty,
    imports: Set[String] = Set.empty,
    globalImports: Set[String] = Set.empty
  ): CSharpProgramSummary =
    new CSharpProgramSummary(namespaceToType, imports, globalImports)

  def apply(summaries: Iterable[CSharpProgramSummary]): CSharpProgramSummary =
    summaries.foldLeft(CSharpProgramSummary())(_ ++= _)

  private val logger = LoggerFactory.getLogger(getClass)

  /** @return
    *   a mapping of the `System` package types.
    */
  private def BuiltinTypes: NamespaceToTypeMap = {
    jsonToInitialMapping(mergeBuiltInTypesJson) match {
      case Failure(exception) =>
        logger.warn("Unable to parse JSON type entry from builtin types", exception); mutable.Map.empty
      case Success(mapping) => mapping
    }
  }

  /** Returns the `CSharpProgramSummary` for the builtin types bundle.
    */
  def builtinTypesSummary: CSharpProgramSummary =
    CSharpProgramSummary(BuiltinTypes)

  /** Returns the `CSharpProgramSummary` for the given JSON file paths.
    *
    * @param paths
    *   the JSON file paths to load types from
    */
  def externalTypesSummary(paths: Set[String]): CSharpProgramSummary =
    CSharpProgramSummary(fromExternalJsons(paths))

  private def fromExternalJsons(paths: Set[String]): NamespaceToTypeMap = {
    val jsonFiles = paths.flatMap(SourceFiles.determine(_, Set(".json"))(VisitOptions.default)).toList
    val inputStreams = jsonFiles.flatMap { path =>
      Try(java.io.FileInputStream(path)) match {
        case Success(stream) => Some(stream)
        case Failure(exc) =>
          logger.warn(s"Unable to open file: $path", exc)
          None
      }
    }

    if (inputStreams.isEmpty) {
      logger.warn("No JSON files found in the provided paths.")
      mutable.Map.empty
    } else {
      jsonToInitialMapping(loadAndMergeJsonStreams(inputStreams)) match {
        case Success(mapping) => mapping
        case Failure(exception) =>
          logger.warn("Failed to parsed merged JSON streams", exception)
          mutable.Map.empty
      }
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

  private def loadAndMergeJsonStreams(jsonInputStreams: List[InputStream]): InputStream = {
    val jsonObjects = for {
      inputStream <- jsonInputStreams
      jsonString = Source.fromInputStream(inputStream).mkString
      jsonObject = ujson.read(jsonString).obj
    } yield jsonObject

    val mergedJson = jsonObjects
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
      loadAndMergeJsonStreams(resourcePaths.map(classLoader.getResourceAsStream))
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
