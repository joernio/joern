package io.joern.csharpsrc2cpg

import com.typesafe.config.impl.*
import com.typesafe.config.{Config, ConfigFactory}
import io.joern.csharpsrc2cpg.astcreation.AstCreatorHelper
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{
  ClassDeclaration,
  FieldDeclaration,
  InterfaceDeclaration,
  MethodDeclaration,
  NamespaceDeclaration,
  RecordDeclaration,
  StructDeclaration
}
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetJsonParser, DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.astgen.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.datastructures.Stack.Stack
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.InputStream
import java.nio.file.Paths
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

type NamespaceToTypeMap = Map[String, Set[CSharpType]]

/** A mapping of type stubs of known types within the scope of the analysis.
  *
  * @param astGenResult
  *   the parsed application code.
  * @param initialMappings
  *   any additional mappings to add to the scope.
  * @see
  *   [[io.joern.csharpsrc2cpg.TypeMap.jsonToInitialMapping]] for generating initial mappings.
  */
class TypeMap(astGenResult: AstGenRunnerResult, initialMappings: List[NamespaceToTypeMap] = List.empty) {

  private val logger = LoggerFactory.getLogger(getClass)

  private def builtinTypes: NamespaceToTypeMap =
    jsonToInitialMapping(getClass.getResourceAsStream("/builtin_types.json")) match
      case Failure(exception) => logger.warn("Unable to parse JSON type entry from builtin types", exception); Map.empty
      case Success(mapping)   => mapping

  /** Converts a JSON type mapping to a NamespaceToTypeMap entry.
    * @param jsonInputStream
    *   a JSON file as an input stream.
    * @return
    *   the resulting type map in a Try
    */
  def jsonToInitialMapping(jsonInputStream: InputStream): Try[NamespaceToTypeMap] =
    Try(read[NamespaceToTypeMap](ujson.Readable.fromByteArray(jsonInputStream.readAllBytes())))

  private val namespaceToType: NamespaceToTypeMap = {
    def typeMapTasks = astGenResult.parsedFiles.map { file =>
      val parserResult    = DotNetJsonParser.readFile(Paths.get(file))
      val compilationUnit = AstCreatorHelper.createDotNetNodeInfo(parserResult.json(ParserKeys.AstRoot))
      () => parseCompilationUnit(compilationUnit)
    }.iterator
    val typeMaps = ConcurrentTaskUtil.runUsingSpliterator(typeMapTasks).flatMap(_.toOption)
    (builtinTypes +: typeMaps ++: initialMappings).foldLeft(Map.empty[String, Set[CSharpType]])((a, b) => {
      val accumulator = mutable.HashMap.from(a)
      val allKeys     = accumulator.keySet ++ b.keySet

      allKeys.foreach(k =>
        accumulator.updateWith(k) {
          case Some(existing) => Option(a.getOrElse(k, Set.empty) ++ b.getOrElse(k, Set.empty) ++ existing)
          case None           => Option(a.getOrElse(k, Set.empty) ++ b.getOrElse(k, Set.empty))
        }
      )
      accumulator.toMap
    })
  }

  /** For the given namespace, returns the declared classes.
    */
  def classesIn(namespace: String): Set[CSharpType] = namespaceToType.getOrElse(namespace, Set.empty)

  def findType(typeFullName: String): Option[CSharpType] = namespaceToType.values.flatten.find(_.name == typeFullName)

  /** For a class, will search for the associated namespace.
    */
  def namespaceFor(clazz: CSharpType): Option[String] =
    namespaceToType.find { case (_, v) => v.contains(clazz) }.map(_._1)

  /** Parses a compilation unit and returns a mapping from all the contained namespaces and the immediate children
    * types.
    */
  private def parseCompilationUnit(cu: DotNetNodeInfo): Map[String, Set[CSharpType]] = {
    cu.json(ParserKeys.Members)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .filter { x =>
        x.node match
          case NamespaceDeclaration => true
          case _                    => false
      }
      .map(parseNamespace)
      .toMap
  }

  private def parseNamespace(namespace: DotNetNodeInfo): (String, Set[CSharpType]) = {
    val namespaceName = AstCreatorHelper.nameFromNode(namespace)
    val classes = namespace
      .json(ParserKeys.Members)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .filter { x =>
        x.node match
          case ClassDeclaration     => true
          case StructDeclaration    => true
          case RecordDeclaration    => true
          case InterfaceDeclaration => true
          case _                    => false
      }
      .map(classDecl => parseClassDeclaration(classDecl, namespaceName))
      .toSet

    namespaceName -> classes
  }

  private def parseClassDeclaration(classDecl: DotNetNodeInfo, namespaceName: String): CSharpType = {
    val className = AstCreatorHelper.nameFromNode(classDecl)
    val fullName  = s"${namespaceName}.${className}"
    val members = classDecl
      .json(ParserKeys.Members)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .flatMap { x =>
        x.node match
          case MethodDeclaration => parseMethodDeclaration(x)
          case FieldDeclaration  => parseFieldDeclaration(x)
          case _                 => List.empty
      }
      .toList
    CSharpType(fullName, members.collectAll[CSharpMethod].l, members.collectAll[CSharpField].l)
  }

  private def parseMethodDeclaration(methodDecl: DotNetNodeInfo): List[CSharpMethod] = {
    val params = methodDecl
      .json(ParserKeys.ParameterList)
      .obj(ParserKeys.Parameters)
      .arr

    val methodReturn = AstCreatorHelper.createDotNetNodeInfo(methodDecl.json(ParserKeys.ReturnType)).code
    val paramTypes = params
      .map(param => AstCreatorHelper.createDotNetNodeInfo(param))
      .map { param =>
        val typ  = param.json(ParserKeys.Type)(ParserKeys.Keyword)(ParserKeys.Value).str
        val name = param.json(ParserKeys.Identifier)(ParserKeys.Value).str
        (name, typ)
      }

    List(CSharpMethod(AstCreatorHelper.nameFromNode(methodDecl), methodReturn, paramTypes.toList))
  }

  private def parseFieldDeclaration(fieldDecl: DotNetNodeInfo): List[CSharpField] = {
    val declarationNode = AstCreatorHelper.createDotNetNodeInfo(fieldDecl.json(ParserKeys.Declaration))
    declarationNode
      .json(ParserKeys.Variables)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .map(AstCreatorHelper.nameFromNode)
      .map(CSharpField.apply)
      .toList
  }

}

case class CSharpField(name: String) derives ReadWriter

case class CSharpMethod(name: String, returnType: String, parameterTypes: List[(String, String)]) derives ReadWriter

case class CSharpType(name: String, methods: List[CSharpMethod], fields: List[CSharpField]) derives ReadWriter
