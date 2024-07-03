package io.joern.x2cpg.frontendspecific.php2cpg

import better.files.File
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.passes.frontend.{TypeStubsParserConfig, XTypeStubsParserConfig}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import org.slf4j.{Logger, LoggerFactory}
import scopt.OParser

import java.io.File as JFile
import java.nio.file.Paths
import scala.io.Source

// Corresponds to a parsed row in the known functions file
case class KnownFunction(
  name: String,
  // return types. A function has at most one return value, but with one or more types.
  rTypes: Seq[String] = Seq.empty,
  // Index 0 = parameter at P0. A function has potentially multiple parameters, each with one or more types.
  pTypes: Seq[Seq[String]] = Seq.empty
)

/** Sets the return and parameter types for builtin functions with known function signatures.
  *
  * TODO: Need to handle variadic arguments.
  */
class PhpTypeStubsParserPass(cpg: Cpg, config: XTypeStubsParserConfig = XTypeStubsParserConfig())
    extends ForkJoinParallelCpgPass[KnownFunction](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[KnownFunction] = {
    /* parse file and return each row as a KnownFunction object */
    val typeStubsFile = config.typeStubsFilePath
    val source = typeStubsFile match {
      case Some(file) => Source.fromFile(file)
      case _          => Source.fromResource("php_known_function_signatures.txt")
    }
    val contents = source.getLines().filterNot(_.startsWith("//"))
    val arr      = contents.flatMap(line => createKnownFunctionFromLine(line)).toArray
    source.close
    arr
  }

  override def runOnPart(builder: DiffGraphBuilder, part: KnownFunction): Unit = {
    /* calculate the result of this part - this is done as a concurrent task */
    val builtinMethod = cpg.method.fullNameExact(part.name).l
    builtinMethod.foreach(mNode => {
      setTypes(builder, mNode.methodReturn, part.rTypes)
      (mNode.parameter.l zip part.pTypes).map((p, pTypes) => setTypes(builder, p, pTypes))
    })
  }

  def createKnownFunctionFromLine(line: String): Option[KnownFunction] = {
    line.split(";").map(_.strip).toList match {
      case Nil                      => None
      case name :: Nil              => Some(KnownFunction(name))
      case name :: rTypes :: Nil    => Some(KnownFunction(name, scanReturnTypes(rTypes)))
      case name :: rTypes :: pTypes => Some(KnownFunction(name, scanReturnTypes(rTypes), scanParamTypes(pTypes)))
    }
  }

  /* From comma separated list of types, create list of types. */
  def scanReturnTypes(rTypesRaw: String): Seq[String] = rTypesRaw.split(",").map(_.strip).toSeq

  /* From a semicolon separated list of parameters, each with a comma separated list of types,
   * create a list of lists of types. */
  def scanParamTypes(pTypesRawArr: List[String]): Seq[Seq[String]] =
    pTypesRawArr.map(paramTypeRaw => paramTypeRaw.split(",").map(_.strip).toSeq).toSeq

  protected def setTypes(builder: DiffGraphBuilder, n: StoredNode, types: Seq[String]): Unit =
    if (types.size == 1) builder.setNodeProperty(n, PropertyNames.TYPE_FULL_NAME, types.head)
    else builder.setNodeProperty(n, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types)
}
