package io.joern.javasrc2cpg.typesolvers

import better.files.File
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.resolution.TypeSolver
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.model.SymbolReference
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.{JavaParser, ParserConfiguration}
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.{ByteArrayInputStream, FileInputStream, InputStream, StringReader}
import scala.annotation.targetName
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

/** A type solver built out of a [[JavaProgramSummary]].
  *
  * Proceeds similarly to [[EagerSourceTypeSolver]], by first transforming type summaries into equivalent Java
  * declarations to be then parsed.
  */
class JavaSummaryTypeSolver(
  summary: JavaProgramSummary,
  combinedTypeSolver: SimpleCombinedTypeSolver,
  symbolSolver: JavaSymbolSolver
) extends TypeSolver {

  private val logger             = LoggerFactory.getLogger(getClass)
  private var parent: TypeSolver = _
  private val javaParser: JavaParser = new JavaParser(
    new ParserConfiguration()
      .setLanguageLevel(LanguageLevel.BLEEDING_EDGE)
      .setStoreTokens(false)
  )

  private def parseSummary(javaCode: String): Option[CompilationUnit] = {
    val parseResult = javaParser.parse(StringReader(javaCode))
    parseResult.getResult.toScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED => Some(result)
      case _ =>
        logger.warn(s"Encountered problems while parsing a Java type summary:")
        parseResult.getProblems.asScala.foreach(problem => logger.warn(s"- ${problem.getMessage}"))
        None
    }
  }

  private def resolveSymbols(cu: CompilationUnit): List[(String, SymbolReference[ResolvedReferenceTypeDeclaration])] = {
    symbolSolver.inject(cu)
    cu.findAll(classOf[TypeDeclaration[_]])
      .asScala
      .map { typeDeclaration =>
        val name = typeDeclaration.getFullyQualifiedName.toScala.getOrElse(typeDeclaration.getNameAsString)
        val resolvedSymbol = Try(
          SymbolReference.solved(
            JavaParserFacade.get(combinedTypeSolver).getTypeDeclaration(typeDeclaration)
          ): SymbolReference[ResolvedReferenceTypeDeclaration]
        ).getOrElse(SymbolReference.unsolved())
        name -> resolvedSymbol
      }
      .toList
  }

  private val foundTypes: Map[String, SymbolReference[ResolvedReferenceTypeDeclaration]] =
    summary.asJavaCode.flatMap(parseSummary).flatMap(resolveSymbols).toMap

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] =
    foundTypes.getOrElse(name, SymbolReference.unsolved())

  override def getParent: TypeSolver = parent

  override def setParent(parent: TypeSolver): Unit = {
    if (parent == null) {
      logger.warn(s"Cannot set parent of type solver to null. setParent will be ignored.")
    } else if (this.parent != null) {
      logger.warn(s"Attempting to re-set type solver parent. setParent will be ignored.")
    } else if (parent == this) {
      logger.warn(s"Parent of TypeSolver cannot be itself. setParent will be ignored.")
    } else {
      this.parent = parent
    }
  }
}

case class JavaFieldSummary(name: String, typeName: String) extends FieldLike derives ReadWriter {
  def asJavaCode: List[String] = s"public $typeName $name;" :: Nil
}

case class JavaMethodSummary(
  name: String,
  returnType: String,
  parameterTypes: List[(String, String)],
  isStatic: Boolean
) extends MethodLike
    derives ReadWriter {

  def asJavaCode: List[String] =
    s"""
       |public${if (isStatic) " static" else ""} $returnType $name($parametersAsJavaCode){}
       |""".stripMargin :: Nil

  private def parametersAsJavaCode: String = parameterTypes.map { p => s"${p._2} ${p._1}" }.mkString(", ")
}

case class JavaTypeSummary(
  name: String,
  methods: List[JavaMethodSummary],
  fields: List[JavaFieldSummary],
  isStatic: Boolean,
  innerClasses: List[JavaTypeSummary]
) extends TypeLike[JavaMethodSummary, JavaFieldSummary]
    derives ReadWriter {

  @targetName("add")
  override def +(other: TypeLike[JavaMethodSummary, JavaFieldSummary]): TypeLike[JavaMethodSummary, JavaFieldSummary] =
    this.copy(methods = mergeMethods(other), fields = mergeFields(other))

  def asJavaCode: List[String] =
    s"""
       |public${if (isStatic) " static" else ""} class $name{
       |${fields.flatMap(_.asJavaCode).mkString("\n")}
       |${methods.flatMap(_.asJavaCode).mkString("\n")}
       |${innerClasses.flatMap(_.asJavaCode).mkString("\n")}
       |}""".stripMargin :: Nil
}

type NamespaceToTypeMap = Map[String, Set[JavaTypeSummary]]

class JavaProgramSummary(initialMappings: List[NamespaceToTypeMap] = List.empty)
    extends ProgramSummary[JavaTypeSummary] {

  override val namespaceToType: NamespaceToTypeMap = initialMappings.reduceOption(_ ++ _).getOrElse(Map.empty)

  @targetName("add")
  def ++(other: JavaProgramSummary): JavaProgramSummary = JavaProgramSummary(
    ProgramSummary.combine(this.namespaceToType, other.namespaceToType) :: Nil
  )

  def asJavaCode: List[String] = namespaceToType.flatMap { (nsName, types) =>
    types.map { typeSummary =>
      s"""
         |package $nsName;
         |${typeSummary.asJavaCode.mkString}
         |""".stripMargin
    }
  }.toList
}

object JavaProgramSummary {

  private val logger = LoggerFactory.getLogger(getClass)

  def empty: JavaProgramSummary = JavaProgramSummary(List.empty)

  private def fromJsonInputStream(jsonInputStream: InputStream): Try[NamespaceToTypeMap] =
    Try(read[NamespaceToTypeMap](ujson.Readable.fromByteArray(jsonInputStream.readAllBytes())))

  def fromJsonString(jsonString: String): Try[NamespaceToTypeMap] =
    fromJsonInputStream(ByteArrayInputStream(jsonString.getBytes))

  def fromJsonDirectoryPath(path: String): List[NamespaceToTypeMap] = Try {
    File(path)
      .collectChildren(_.name.endsWith(".json"))
      .flatMap { jsonFile =>
        fromJsonInputStream(FileInputStream(jsonFile.toJava)) match {
          case Success(mappings) => mappings :: Nil
          case Failure(err) =>
            logger.warn(s"Could not load Java type summary `${jsonFile.path}`:")
            logger.warn(err.getMessage)
            Nil
        }
      }
      .toList
  }.getOrElse(Nil)

}
