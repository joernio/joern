package io.joern.dataflowengineoss

import better.files.File
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.PropertyKey
import upickle.default.*

import java.util.concurrent.{ExecutorService, Executors}
import java.util.regex.Pattern

package object slicing {

  trait BaseConfig[T <: BaseConfig[T]] {

    var inputPath: File = File("cpg.bin")

    var outputSliceFile: File = File("slices")

    var dummyTypesEnabled: Boolean = false

    var fileFilter: Option[String] = None

    var methodNameFilter: Option[String] = None

    var methodParamTypeFilter: Option[String] = None

    var methodAnnotationFilter: Option[String] = None

    var parallelism: Option[Int] = None

    def withInputPath(x: File): T = {
      this.inputPath = x
      this.asInstanceOf[T]
    }

    def withOutputSliceFile(x: File): T = {
      this.outputSliceFile = x
      this.asInstanceOf[T]
    }

    def withDummyTypesEnabled(x: Boolean): T = {
      this.dummyTypesEnabled = x
      this.asInstanceOf[T]
    }

    def withFileFilter(x: Option[String]): T = {
      this.fileFilter = x
      this.asInstanceOf[T]
    }

    def withMethodNameFilter(x: Option[String]): T = {
      this.methodNameFilter = x
      this.asInstanceOf[T]
    }

    def withMethodParamTypeFilter(x: Option[String]): T = {
      this.methodParamTypeFilter = x
      this.asInstanceOf[T]
    }

    def withMethodAnnotationFilter(x: Option[String]): T = {
      this.methodParamTypeFilter = x
      this.asInstanceOf[T]
    }

    def withParallelism(x: Int): T = {
      this.parallelism = Option(x)
      this.asInstanceOf[T]
    }

  }

  case class DefaultSliceConfig() extends BaseConfig[DefaultSliceConfig]

  case class DataFlowConfig(
    sinkPatternFilter: Option[String] = None,
    mustEndAtExternalMethod: Boolean = false,
    sliceDepth: Int = 20
  ) extends BaseConfig[DataFlowConfig]

  case class UsagesConfig(
    minNumCalls: Int = 1,
    excludeOperatorCalls: Boolean = false,
    excludeMethodSource: Boolean = false
  ) extends BaseConfig[UsagesConfig]

  /** Adds extensions to modify a call traversal based on config options.
    */
  implicit class CallFilterExt(trav: Iterator[Call]) {

    /** This works because we use backwards slicing and start at sinks.
      */
    def withExternalCalleeFilter(implicit config: DataFlowConfig, resolver: ICallResolver): Iterator[Call] =
      if (config.mustEndAtExternalMethod) trav.where(_.callee.filter(_.isExternal))
      else trav

    def withSinkFilter(implicit config: DataFlowConfig): Iterator[Call] = {
      config.sinkPatternFilter match {
        case Some(pattern) => trav.code(pattern)
        case None          => trav
      }
    }

  }

  /** Adds extensions to modify a method traversal based on config options
    */
  implicit class MethodFilterExt(trav: Iterator[Method]) {

    def withMethodNameFilter(implicit config: BaseConfig[?]): Iterator[Method] = config.methodNameFilter match {
      case Some(filter) => trav.name(filter)
      case None         => trav
    }

    def withMethodParameterFilter(implicit config: BaseConfig[?]): Iterator[Method] =
      config.methodParamTypeFilter match {
        case Some(filter) => trav.where(_.parameter.evalType(filter))
        case None         => trav
      }

    def withMethodAnnotationFilter(implicit config: BaseConfig[?]): Iterator[Method] =
      config.methodAnnotationFilter match {
        case Some(filter) => trav.where(_.annotation.code(filter))
        case None         => trav
      }

  }

  /** A trait for all objects that represent a 1:1 relationship between the CPG and all the slices extracted.
    */
  sealed trait ProgramSlice {

    def toJson: String

    def toJsonPretty: String

  }

  /** A data-flow slice vector for a given backwards intraprocedural path.
    *
    * @param nodes
    *   the nodes in the slice.
    * @param edges
    *   a map linking nodes with their edges.
    */
  case class DataFlowSlice(nodes: Set[SliceNode], edges: Set[SliceEdge]) extends ProgramSlice derives ReadWriter {

    def toJson: String = write(this)

    def toJsonPretty: String = write(this, indent = 2, sortKeys = true)
  }

  case class SliceNode(
    id: Long,
    label: String,
    name: String = "",
    code: String,
    typeFullName: String = "",
    parentMethod: String = "",
    parentFile: String = "",
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ) derives ReadWriter

  case class SliceEdge(src: Long, dst: Long, label: String) derives ReadWriter

  /** A usage slice of an object at the start of its definition until its final usage.
    *
    * @param targetObj
    *   the name and type of the focus object.
    * @param definedBy
    *   the name of the call, identifier, or literal that defined the target object, if available.
    * @param invokedCalls
    *   calls this object is observed to call.
    * @param argToCalls
    *   the calls this object is observed to be an argument of.
    */
  case class ObjectUsageSlice(
    targetObj: DefComponent,
    definedBy: Option[DefComponent],
    invokedCalls: List[ObservedCall],
    argToCalls: List[ObservedCallWithArgPos]
  ) derives ReadWriter {
    override def toString: String =
      s"{tgt: $targetObj${definedBy.map(p => s" = $p").getOrElse("")}, " +
        s"inv: [${invokedCalls.mkString(",")}], " +
        s"argsTo: [${argToCalls.mkString(",")}]" +
        s"}"
  }

  /** Packages the object usage slices along with the method source code.
    *
    * @param code
    *   raw source code.
    * @param fullName
    *   method full name.
    * @param fileName
    *   the file name.
    * @param slices
    *   the object usage slices.
    */
  case class MethodUsageSlice(
    code: String,
    fullName: String,
    fileName: String,
    slices: Set[ObjectUsageSlice],
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ) derives ReadWriter

  /** Represents a source of data-generation, i.e., where data is defined and can be assigned to some variable or used
    * in an argument.
    */
  sealed trait DefComponent {
    def name: String

    def typeFullName: String

    def label: String

    def lineNumber: Option[Int]

    def columnNumber: Option[Int]

    override def toString: String = s"[$label] $name" + (if (typeFullName.nonEmpty) s": $typeFullName" else "")

  }

  /** Represents a local transfer of data via aliasing. The data defined is via some alias.
    */
  case class LocalDef(
    name: String,
    typeFullName: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "LOCAL"
  ) extends DefComponent
      derives ReadWriter

  /** Represents a literal.
    */
  case class LiteralDef(
    name: String,
    typeFullName: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "LITERAL"
  ) extends DefComponent
      derives ReadWriter

  /** Represents data introduced via a parameter.
    *
    * @param position
    *   the index of the parameter.
    */
  case class ParamDef(
    name: String,
    typeFullName: String,
    position: Integer,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "PARAM"
  ) extends DefComponent
      derives ReadWriter {
    override def toString: String = super.toString + s" @ pos #$position"
  }

  /** Represents data introduced by the return value of a call.
    *
    * @param resolvedMethod
    *   the full method path if resolved.
    */
  case class CallDef(
    name: String,
    typeFullName: String,
    resolvedMethod: Option[String] = None,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "CALL"
  ) extends DefComponent
      derives ReadWriter {
    override def toString: String = super.toString + resolvedMethod.map(s => s" @ $s").getOrElse("")
  }

  /** Represents data introduced by an unhandled data structure.
    */
  case class UnknownDef(
    name: String,
    typeFullName: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "UNKNOWN"
  ) extends DefComponent
      derives ReadWriter

  // The following encoders make sure the object does follow ClassName: { properties ... } format but instead
  // is just { properties }. This makes it less automatically serializable but we have `label` to encode classes.

  implicit val defComponentRw: ReadWriter[DefComponent] = readwriter[ujson.Value].bimap[DefComponent](
    {
      case local: LocalDef     => write(local)
      case literal: LiteralDef => write(literal)
      case call: CallDef       => write(call)
      case param: ParamDef     => write(param)
      case unknown: UnknownDef => write(unknown)
    },
    json =>
      json("label").strOpt match {
        case Some("LOCAL")   => read[LocalDef](json)
        case Some("LITERAL") => read[LiteralDef](json)
        case Some("CALL")    => read[CallDef](json)
        case Some("PARAM")   => read[ParamDef](json)
        case Some("UNKNOWN") => read[UnknownDef](json)
        case _               => throw new RuntimeException(s"Unable to deserialize the given `DefComponent`: $json")
      }
  )

  object DefComponent {

    val unresolvedCallPattern: Pattern = Pattern.compile("^(<unknown|ANY).*$")
    private val logger                 = LoggerFactory.getLogger(DefComponent.getClass)

    /** Attempts to generate an [[DefComponent]] from the given CPG node.
      *
      * @param node
      *   the CPG node.
      * @return
      *   an ID type pair with default values "UNKNOWN" if the respective properties for [[LocalDef]] could not be
      *   extracted.
      */
    def fromNode(node: StoredNode, typeMap: Map[String, String] = Map.empty[String, String]): DefComponent = {
      val nodeType = (node.property(PropertyNames.TYPE_FULL_NAME, "ANY") +: node.property(
        PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
        Seq.empty[String]
      )).filterNot(_.matches("(ANY|UNKNOWN)")).headOption.getOrElse("ANY")
      val typeFullName = typeMap.getOrElse(nodeType, nodeType)
      val lineNumber   = Option(node.property(new PropertyKey[Integer](PropertyNames.LINE_NUMBER))).map(_.toInt)
      val columnNumber = Option(node.property(new PropertyKey[Integer](PropertyNames.COLUMN_NUMBER))).map(_.toInt)
      node match {
        case x: MethodParameterIn => ParamDef(x.name, typeFullName, x.index, lineNumber, columnNumber)
        case x: Call if x.code.startsWith("new ") =>
          val typeName = x.code.stripPrefix("new ").takeWhile(!_.equals('('))
          CallDef(
            x.code.takeWhile(_ != '('),
            typeMap.getOrElse(typeName, x.typeFullName),
            typeMap.get(typeName),
            lineNumber,
            columnNumber
          )
        case x: Call if unresolvedCallPattern.matcher(x.methodFullName).matches() =>
          CallDef(x.code.takeWhile(_ != '('), typeFullName)
        case x: Call =>
          CallDef(x.code.takeWhile(_ != '('), typeFullName, Option(x.methodFullName), lineNumber, columnNumber)
        case x: Identifier => LocalDef(x.name, typeFullName, lineNumber, columnNumber)
        case x: Local      => LocalDef(x.name, typeFullName, lineNumber, columnNumber)
        case x: Literal    => LiteralDef(x.code, typeFullName, lineNumber, columnNumber)
        case x: Member     => LocalDef(x.name, typeFullName, lineNumber, columnNumber)
        case x: AstNode =>
          logger.warn(s"Unhandled conversion from node type ${x.label} to DefComponent")
          UnknownDef(x.code, typeFullName, lineNumber, columnNumber)
      }
    }
  }

  /** Call details in the usage slice.
    *
    * @param callName
    *   the name of the call.
    * @param resolvedMethod
    *   the method full name if the call is resolved.
    * @param paramTypes
    *   the observed parameter types.
    * @param returnType
    *   the observed return type.
    */
  sealed abstract class UsedCall(
    callName: String,
    resolvedMethod: Option[String],
    paramTypes: List[String],
    returnType: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ) {
    override def toString: String =
      s"$callName(${paramTypes.mkString(",")}):$returnType"
  }

  /** Details related to an observed call.
    */
  case class ObservedCall(
    callName: String,
    resolvedMethod: Option[String],
    paramTypes: List[String],
    returnType: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ) extends UsedCall(callName, resolvedMethod, paramTypes, returnType, lineNumber, columnNumber)
      derives ReadWriter

  /** Extends observed call with a specific argument in mind.
    *
    * @param position
    *   adds the argument position as either a named argument or positional argument.
    */
  case class ObservedCallWithArgPos(
    callName: String,
    resolvedMethod: Option[String],
    paramTypes: List[String],
    returnType: String,
    position: Either[String, Int],
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ) extends UsedCall(callName, resolvedMethod, paramTypes, returnType, lineNumber, columnNumber) {
    override def toString: String = super.toString + " @ " + (position match {
      case Left(namedArg) => namedArg
      case Right(argIdx)  => argIdx
    })
  }

  object ObservedCallWithArgPos {
    def fromObservedCall(oc: ObservedCall, pos: Either[String, Int]): ObservedCallWithArgPos =
      ObservedCallWithArgPos(
        oc.callName,
        oc.resolvedMethod,
        oc.paramTypes,
        oc.returnType,
        pos,
        oc.lineNumber,
        oc.columnNumber
      )
  }

  implicit val observedCallWithArgPosRw: ReadWriter[ObservedCallWithArgPos] =
    readwriter[ujson.Value].bimap[ObservedCallWithArgPos](
      x => {
        val position = x.position match {
          case Left(str)  => ujson.Str(str)
          case Right(num) => ujson.Num(num)
        }
        ujson.Obj(
          "callName"       -> x.callName,
          "resolvedMethod" -> x.resolvedMethod,
          "paramTypes"     -> x.paramTypes,
          "returnType"     -> x.returnType,
          "lineNumber"     -> x.lineNumber,
          "columnNumber"   -> x.columnNumber,
          "position"       -> position
        )
      },
      json => {
        val position =
          if (json("position").strOpt.isDefined) Left(json("position").str)
          else Right(json("position").num.toInt)
        ObservedCallWithArgPos(
          json("callName").str,
          read[Option[String]](json("resolvedMethod")),
          read[List[String]](json("paramTypes")),
          json("returnType").str,
          position,
          read[Option[Int]](json("lineNumber")),
          read[Option[Int]](json("columnNumber"))
        )
      }
    )

  /** Describes types defined within the application.
    *
    * @param name
    *   name of the type.
    * @param fields
    *   the static or object fields.
    * @param procedures
    *   defined, named procedures within the type.
    */
  case class UserDefinedType(
    name: String,
    fields: List[LocalDef],
    procedures: List[ObservedCall],
    fileName: String = "",
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ) derives ReadWriter

  /** The program usage slices and UDTs.
    *
    * @param objectSlices
    *   the object slices under each procedure
    * @param userDefinedTypes
    *   the UDTs.
    */
  case class ProgramUsageSlice(objectSlices: List[MethodUsageSlice], userDefinedTypes: List[UserDefinedType])
      extends ProgramSlice derives ReadWriter {

    def toJson: String = upickle.default.write(this)

    def toJsonPretty: String = upickle.default.write(this, indent = 2, sortKeys = true)
  }

}
