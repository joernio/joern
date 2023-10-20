package io.joern.php2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate

import scala.io.Source
import java.io.{File => JFile}

// Corresponds to a parsed row in the known functions file
case class KnownFunction(
  name: String,
  rTypes: Seq[String],      // return types
  pTypes: Seq[Seq[String]]  // Index 0 = parameter at P0
)

/**
 * Sets the return and parameter types for builtin functions with known function
 * signatures.
 *
 * TODO: Need to handle variadic arguments.
 */
class PhpSetKnownTypesPass(cpg: Cpg, knownTypesFile: Option[JFile] = None)
  extends ForkJoinParallelCpgPass[KnownFunction](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[KnownFunction] = {
    /* parse file and return each row as a KnownFunction object */
    val source = knownTypesFile match {
      case Some(file) => Source.fromFile(file)
      case _ => Source.fromResource("known_function_signatures.txt")
    }
    /* parse file and return each row as a KnownFunction object */
    val contents = source.getLines().filterNot(_.startsWith("//"))
    val arr = contents.map(line => lineToKnownFunction(line)).toArray
    source.close
    arr
  }

  override def runOnPart(builder: overflowdb.BatchedUpdate.DiffGraphBuilder, part: KnownFunction): Unit = {
    /* calculate the result of this part - this is done as a concurrent task */
    val builtinMethod = cpg.method.fullNameExact(part.name).l
    builtinMethod.foreach(mNode => {
      setTypes(builder, mNode.methodReturn, part.rTypes)
    })
  }

  def lineToKnownFunction(line: String): KnownFunction = {
    val fields = line.split(";")
    val name = fields(0)
    fields.length match {
      case length if (length < 2) => {
        logger.warn(s"invalid known function signature for ${name}: no parameter types")
        KnownFunction(fields(0), Seq(), Seq())
      }
      case length if (length < 3) => {
        KnownFunction(fields(0), scanReturnTypes(fields(1)), Seq())
      }
      case length => {
        KnownFunction(fields(0).strip, scanReturnTypes(fields(1)), scanParamTypes(fields.slice(2, length)))
      }
    }
  }

  def scanReturnTypes(rTypesRaw: String): Seq[String] = rTypesRaw.split(",").map(_.strip).toSeq

  def scanParamTypes(pTypesRawArr: Array[String]): Seq[Seq[String]] =
    pTypesRawArr.map(paramTypeRaw => paramTypeRaw.split(",").map(_.strip).toSeq).toSeq

  protected def setTypes(builder: overflowdb.BatchedUpdate.DiffGraphBuilder, n: StoredNode, types: Seq[String]): Unit =
    if (types.size == 1) builder.setNodeProperty(n, PropertyNames.TYPE_FULL_NAME, types.head)
    else builder.setNodeProperty(n, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types)
}

class SetKnownTypes(cpg: Cpg, builder: BatchedUpdate.DiffGraphBuilder) {

  private val logger = LoggerFactory.getLogger(getClass)

  protected val builtinsParams: Map[String, Seq[Set[String]]] =
    KnownFunctionSignatures.Signatures.map(
      l => if (l.length < 3) {
        logger.warn(s"invalid known function signature - no parameter types")
        (l.head.strip, Seq())
      } else {
        l.head.strip -> l.tail.tail.map(
          _.split(",").map(_.strip).toSet
        ).toSeq
      }
    ).toMap

  protected val builtinsReturns: Map[String, Set[String]] =
    KnownFunctionSignatures.Signatures.map(
      l => if (l.length < 2) {
        logger.warn(s"invalid known function signature - no return types")
        (l.head.strip, Set())
      } else {
        l(0).strip -> l(1).split(",").map(_.strip).toSet
      }
    ).toMap

  object KnownFunctionSignatures {
    val Signatures: List[Array[String]] = {
      Source.fromResource("known_function_signatures.txt").getLines()
        .filterNot(_.startsWith("//"))
        .map(_.split(";")).toList
    }
  }

  def setParamTypes(): Unit = {
    builtinsParams.foreach {case (name, params) => setMethodParams(name, params) }
  }

  def setReturnTypes(): Unit = {

    builtinsReturns.foreach {case (name, returnTypes) => setMethodReturns(name, returnTypes) }
  }

  def setMethodParams(name: String, paramTypes: Seq[Set[String]]): Unit = {
    val paramNodes = cpg.method.fullNameExact(name).parameter.l
    logger.debug(s"method ${name} parameters: ${paramNodes.collect(_.name).mkString(", ")}")
    logger.debug(s"- param types: ${paramTypes.map(_.mkString(", ")).mkString("; ")}")
    // Do I know that the paramNodes will be ordered by index?
    val zipped_params = paramNodes zip paramTypes
    logger.debug(s"zipped: ${zipped_params.mkString(", ")}")
    zipped_params.map {
      case (node, paramType) => {
        setParamTypes(node, paramType)
      }
    }
  }

  def setParamTypes(paramNode: MethodParameterIn, types: Set[String]): Unit = {
    logger.debug(s"- setting param ${paramNode.name} to type: [${types.mkString(", ")}]")
    setTypes(paramNode, types.toSeq)
  }

  def setMethodReturns(name: String, returnTypes: Set[String]): Unit = {
    val returnNodes = cpg.method.fullNameExact(name).methodReturn.l
    returnNodes.foreach {
      logger.debug(s"setting method ${name} return types ${returnTypes.mkString(", ")}")
      setTypes(_, returnTypes.toSeq)
    }
  }

  protected def setTypes(n: StoredNode, types: Seq[String]): Unit =
    if (types.size == 1) builder.setNodeProperty(n, PropertyNames.TYPE_FULL_NAME, types.head)
    else builder.setNodeProperty(n, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types)
}