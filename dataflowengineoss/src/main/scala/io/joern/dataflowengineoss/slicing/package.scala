package io.joern.dataflowengineoss

import better.files.File
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.joern.x2cpg.X2CpgConfig
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.PropertyKey

import java.util.regex.Pattern

package object slicing {

  import cats.syntax.functor._
  import io.circe.generic.auto._
  import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
  import io.circe.syntax.EncoderOps

  trait BaseConfig {

    var inputPath: File = File("cpg.bin")

    var outputSliceFile: File = File("slices")

    var dummyTypesEnabled: Boolean = false

    var fileFilter: Option[String] = None

    var methodNameFilter: Option[String] = None

    var methodParamTypeFilter: Option[String] = None

    var methodAnnotationFilter: Option[String] = None

    def withInputPath(x: File): BaseConfig = {
      this.inputPath = x
      this
    }

    def withOutputSliceFile(x: File): BaseConfig = {
      this.outputSliceFile = x
      this
    }

    def withDummyTypesEnabled(x: Boolean): BaseConfig = {
      this.dummyTypesEnabled = x
      this
    }

    def withFileFilter(x: Option[String]): BaseConfig = {
      this.fileFilter = x
      this
    }

    def withMethodNameFilter(x: Option[String]): BaseConfig = {
      this.methodNameFilter = x
      this
    }

    def withMethodParamTypeFilter(x: Option[String]): BaseConfig = {
      this.methodParamTypeFilter = x
      this
    }

    def withMethodAnnotationFilter(x: Option[String]): BaseConfig = {
      this.methodParamTypeFilter = x
      this
    }

  }

  case class DefaultSliceConfig() extends BaseConfig

  case class DataFlowConfig(
    sinkPatternFilter: Option[String] = None,
    mustEndAtExternalMethod: Boolean = false,
    sliceDepth: Int = 20
  ) extends BaseConfig

  case class UsagesConfig(
    minNumCalls: Int = 1,
    excludeOperatorCalls: Boolean = false,
    excludeMethodSource: Boolean = false
  ) extends BaseConfig

  /** Adds extensions to modify a method traversal based on config options
    */
  implicit class MethodFilterExt(trav: Iterator[Method]) {

    def withMethodNameFilter(implicit config: BaseConfig): Iterator[Method] = config.methodNameFilter match {
      case Some(filter) => trav.name(filter)
      case None         => trav
    }

    def withMethodParameterFilter(implicit config: BaseConfig): Iterator[Method] = config.methodParamTypeFilter match {
      case Some(filter) => trav.where(_.parameter.evalType(filter))
      case None         => trav
    }

    def withMethodAnnotationFilter(implicit config: BaseConfig): Iterator[Method] =
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
    * @param methodToChildNode
    *   a mapping between method names and which nodes fall under them.
    */
  case class DataFlowSlice(nodes: Set[SliceNode], edges: Set[SliceEdge], methodToChildNode: Map[String, Set[Long]])
      extends ProgramSlice {
    def toJson: String = this.asJson.toString()

    def toJsonPretty: String = this.asJson.spaces2
  }

  implicit val encodeDataFlowSlice: Encoder[DataFlowSlice] = Encoder.instance {
    case DataFlowSlice(nodes, edges, methodToChildNode) =>
      Json.obj("nodes" -> nodes.asJson, "edges" -> edges.asJson, "methodToChildNode" -> methodToChildNode.asJson)
  }

  case class SliceNode(
    id: Long,
    label: String,
    name: String = "",
    code: String,
    typeFullName: String = "",
    lineNumber: Option[Integer] = None,
    columnNumber: Option[Integer] = None
  )

  implicit val encodeSliceNode: Encoder[SliceNode] = Encoder.instance {
    case SliceNode(id, label, name, code, typeFullName, lineNumber, columnNumber) =>
      Json.obj(
        "id"           -> id.asJson,
        "label"        -> label.asJson,
        "name"         -> name.asJson,
        "code"         -> code.asJson,
        "typeFullName" -> typeFullName.asJson,
        "lineNumber"   -> lineNumber.asJson,
        "columnNumber" -> columnNumber.asJson
      )
  }

  case class SliceEdge(src: Long, dst: Long, label: String)

  implicit val encodeSliceEdge: Encoder[SliceEdge] = Encoder.instance { case SliceEdge(src, dst, label) =>
    Json.obj("src" -> src.asJson, "dst" -> dst.asJson, "label" -> label.asJson)
  }

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
  ) {
    override def toString: String =
      s"{tgt: $targetObj${definedBy.map(p => s" = $p").getOrElse("")}, " +
        s"inv: [${invokedCalls.mkString(",")}], " +
        s"argsTo: [${argToCalls.mkString(",")}]" +
        s"}"
  }

  implicit val decodeObjectUsageSlice: Decoder[ObjectUsageSlice] =
    (c: HCursor) =>
      for {
        x <- c.downField("targetObj").as[DefComponent]
        p <- c.downField("definedBy").as[Option[DefComponent]]
        r <- c.downField("invokedCalls").as[List[ObservedCall]]
        a <- c.downField("argToCalls").as[List[ObservedCallWithArgPos]]
      } yield {
        ObjectUsageSlice(x, p, r, a)
      }
  implicit val encodeObjectUsageSlice: Encoder[ObjectUsageSlice] =
    Encoder.instance { case ObjectUsageSlice(c, p, r, a) =>
      Json.obj("targetObj" -> c.asJson, "definedBy" -> p.asJson, "invokedCalls" -> r.asJson, "argToCalls" -> a.asJson)
    }

  /** Packages the object usage slices along with the method source code.
    *
    * @param source
    *   source code.
    * @param slices
    *   the object usage slices.
    */
  case class MethodUsageSlice(source: String, slices: Set[ObjectUsageSlice])

  implicit val decodeMethodUsageSlice: Decoder[MethodUsageSlice] =
    (c: HCursor) =>
      for {
        x <- c.downField("source").as[String]
        y <- c.downField("slices").as[Set[ObjectUsageSlice]]
      } yield {
        MethodUsageSlice(x, y)
      }
  implicit val encodeMethodUsageSlice: Encoder[MethodUsageSlice] =
    Encoder.instance { case MethodUsageSlice(x, y) =>
      Json.obj("source" -> x.asJson, "slices" -> y.asJson)
    }

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

  implicit val localDefDecoder: Decoder[LocalDef] = deriveDecoder[LocalDef]
  implicit val localDefEncoder: Encoder[LocalDef] = deriveEncoder[LocalDef]

  /** Represents a literal.
    */
  case class LiteralDef(
    name: String,
    typeFullName: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "LITERAL"
  ) extends DefComponent

  implicit val literalDefDecoder: Decoder[LiteralDef] = deriveDecoder[LiteralDef]
  implicit val literalDefEncoder: Encoder[LiteralDef] = deriveEncoder[LiteralDef]

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
  ) extends DefComponent {
    override def toString: String = super.toString + s" @ pos #$position"
  }

  implicit val paramDefDecoder: Decoder[ParamDef] = deriveDecoder[ParamDef]
  implicit val paramDefEncoder: Encoder[ParamDef] = deriveEncoder[ParamDef]

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
  ) extends DefComponent {
    override def toString: String = super.toString + resolvedMethod.map(s => s" @ $s").getOrElse("")
  }

  implicit val callDefDecoder: Decoder[CallDef] = deriveDecoder[CallDef]
  implicit val callDefEncoder: Encoder[CallDef] = deriveEncoder[CallDef]

  /** Representds data introduced by an unhandled data structure.
    */
  case class UnknownDef(
    name: String,
    typeFullName: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    label: String = "UNKNOWN"
  ) extends DefComponent

  implicit val unknownDefDecoder: Decoder[UnknownDef] = deriveDecoder[UnknownDef]
  implicit val unknownDefEncoder: Encoder[UnknownDef] = deriveEncoder[UnknownDef]

  // The following encoders make sure the object does follow ClassName: { properties ... } format but instead
  // is just { properties }. This makes it less automatically serializable but we have `label` to encode classes.

  implicit val encodeDefComponent: Encoder[DefComponent] = Encoder.instance {
    case local @ LocalDef(_, _, _, _, _)     => local.asJson
    case literal @ LiteralDef(_, _, _, _, _) => literal.asJson
    case call @ CallDef(_, _, _, _, _, _)    => call.asJson
    case param @ ParamDef(_, _, _, _, _, _)  => param.asJson
    case unknown @ UnknownDef(_, _, _, _, _) => unknown.asJson
  }

  implicit val decodeDefComponent: Decoder[DefComponent] =
    List[Decoder[DefComponent]](
      Decoder[LocalDef].widen,
      Decoder[LiteralDef].widen,
      Decoder[CallDef].widen,
      Decoder[ParamDef].widen,
      Decoder[UnknownDef].widen
    ).reduceLeft(_ or _)

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
    returnType: String
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
    returnType: String
  ) extends UsedCall(callName, resolvedMethod, paramTypes, returnType)

  implicit val decodeObservedCall: Decoder[ObservedCall] =
    (c: HCursor) =>
      for {
        x <- c.downField("callName").as[String]
        m <- c.downField("resolvedMethod").as[Option[String]]
        p <- c.downField("paramTypes").as[List[String]]
        r <- c.downField("returnType").as[String]
      } yield {
        ObservedCall(x, m, p, r)
      }
  implicit val encodeObservedCall: Encoder[ObservedCall] =
    Encoder.instance { case ObservedCall(c, m, p, r) =>
      Json.obj("callName" -> c.asJson, "resolvedMethod" -> m.asJson, "paramTypes" -> p.asJson, "returnType" -> r.asJson)
    }

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
    position: Either[String, Int]
  ) extends UsedCall(callName, resolvedMethod, paramTypes, returnType) {
    override def toString: String = super.toString + " @ " + (position match {
      case Left(namedArg) => namedArg
      case Right(argIdx)  => argIdx
    })
  }

  object ObservedCallWithArgPos {
    def fromObservedCall(oc: ObservedCall, pos: Either[String, Int]): ObservedCallWithArgPos =
      ObservedCallWithArgPos(oc.callName, oc.resolvedMethod, oc.paramTypes, oc.returnType, pos)
  }

  implicit val decodeObservedCallWithArgPos: Decoder[ObservedCallWithArgPos] =
    (c: HCursor) =>
      for {
        x <- c.downField("callName").as[String]
        m <- c.downField("resolvedMethod").as[Option[String]]
        p <- c.downField("paramTypes").as[List[String]]
        r <- c.downField("returnType").as[String]
      } yield {
        val pos = c.downField("position").as[Int] match {
          case Left(_) =>
            c.downField("position").as[String] match {
              case Left(err) =>
                throw new RuntimeException(
                  "Unable to decode `position` as the field is neither a string nor an integer",
                  err
                )
              case Right(argName) => Left(argName)
            }
          case Right(argIdx) => Right(argIdx)
        }
        ObservedCallWithArgPos(x, m, p, r, pos)
      }
  implicit val encodeObservedCallWithArgPos: Encoder[ObservedCallWithArgPos] =
    Encoder.instance { case ObservedCallWithArgPos(c, m, p, r, a) =>
      Json.obj(
        "callName"       -> c.asJson,
        "resolvedMethod" -> m.asJson,
        "paramTypes"     -> p.asJson,
        "returnType"     -> r.asJson,
        "position" -> (a match {
          case Left(argName) => argName.asJson
          case Right(argIdx) => argIdx.asJson
        })
      )
    }

  implicit val encodeUsedCall: Encoder[UsedCall] = Encoder.instance {
    case oc @ ObservedCall(_, _, _, _)               => oc.asJson
    case oca @ ObservedCallWithArgPos(_, _, _, _, _) => oca.asJson
  }

  implicit val decodeUsedCall: Decoder[UsedCall] =
    List[Decoder[UsedCall]](Decoder[ObservedCall].widen, Decoder[ObservedCallWithArgPos].widen).reduceLeft(_ or _)

  /** Describes types defined within the application.
    *
    * @param name
    *   name of the type.
    * @param fields
    *   the static or object fields.
    * @param procedures
    *   defined, named procedures within the type.
    */
  case class UserDefinedType(name: String, fields: List[LocalDef], procedures: List[ObservedCall])

  implicit val decodeUserDefinedType: Decoder[UserDefinedType] =
    (c: HCursor) =>
      for {
        n <- c.downField("name").as[String]
        f <- c.downField("fields").as[List[LocalDef]]
        p <- c.downField("procedures").as[List[ObservedCall]]
      } yield {
        UserDefinedType(n, f, p)
      }
  implicit val encodeUserDefinedType: Encoder[UserDefinedType] =
    Encoder.instance { case UserDefinedType(n, f, p) =>
      Json.obj("name" -> n.asJson, "fields" -> f.asJson, "procedures" -> p.asJson)
    }

  /** The program usage slices and UDTs.
    *
    * @param objectSlices
    *   the object slices under each procedure
    * @param userDefinedTypes
    *   the UDTs.
    */
  case class ProgramUsageSlice(objectSlices: Map[String, MethodUsageSlice], userDefinedTypes: List[UserDefinedType])
      extends ProgramSlice {

    def toJson: String = this.asJson.toString()

    def toJsonPretty: String = this.asJson.spaces2
  }

  implicit val decodeProgramUsageSlice: Decoder[ProgramUsageSlice] =
    (c: HCursor) =>
      for {
        o <- c.downField("objectSlices").as[Map[String, MethodUsageSlice]]
        u <- c.downField("userDefinedTypes").as[List[UserDefinedType]]
      } yield {
        ProgramUsageSlice(o, u)
      }
  implicit val encodeProgramUsageSlice: Encoder[ProgramUsageSlice] = Encoder.instance {
    case ProgramUsageSlice(os, udts) => Json.obj("objectSlices" -> os.asJson, "userDefinedTypes" -> udts.asJson)
  }
}
