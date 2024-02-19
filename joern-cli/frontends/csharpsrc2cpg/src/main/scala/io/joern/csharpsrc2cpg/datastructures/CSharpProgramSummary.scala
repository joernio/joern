package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.Constants
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.InputStream
import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

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
  def BuiltinTypes: NamespaceToTypeMap =
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

}

case class CSharpField(name: String, typeName: String) extends FieldLike derives ReadWriter

case class CSharpMethod(name: String, returnType: String, parameterTypes: List[(String, String)], isStatic: Boolean)
    extends MethodLike derives ReadWriter

case class CSharpType(name: String, methods: List[CSharpMethod], fields: List[CSharpField])
    extends TypeLike[CSharpMethod, CSharpField] derives ReadWriter
