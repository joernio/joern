package io.joern.php2cpg.parser

import io.joern.php2cpg.parser.Domain.PhpAssignment.{AssignTypeMap, isAssignType}
import io.joern.php2cpg.parser.Domain.PhpBinaryOp.{BinaryOpTypeMap, isBinaryOpType}
import io.joern.php2cpg.parser.Domain.PhpUnaryOp.{UnaryOpTypeMap, isUnaryOpType}
import io.shiftleft.codepropertygraph.generated.Operators
import org.slf4j.LoggerFactory
import ujson.{Arr, Obj, Str, Value}

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Success, Try}

object Domain {

  object PhpOperators {
    // TODO Decide which of these should be moved to codepropertygraph
    val coalesce     = "<operator>.coalesce"
    val concat       = "<operator>.concat"
    val identical    = "<operator>.identical"
    val logicalXor   = "<operator>.logicalXor"
    val notIdentical = "<operator>.notIdentical"
    val spaceship    = "<operator>.spaceship"

    val assignmentCoalesce = "<operator>.assignmentCoalesce"
    val assignmentConcat   = "<operator>.assignmentConcat"

    val encaps = "<operator>.encaps"
  }

  private val logger                      = LoggerFactory.getLogger(Domain.getClass)
  private val NamespaceDelimiter          = "."
  private val FullyQualifiedNameDelimiter = "."

  final case class PhpAttributes(lineNumber: Option[Integer], kind: Option[Int])
  object PhpAttributes {
    val Empty: PhpAttributes = PhpAttributes(None, None)

    def apply(json: Value): PhpAttributes = {
      Try(json("attributes")) match {
        case Success(Obj(attributes)) =>
          val startLine = attributes.get("startLine").map(num => Integer.valueOf(num.num.toInt))
          val kind      = attributes.get("kind").map(_.num.toInt)
          PhpAttributes(startLine, kind)

        case unhandled =>
          logger.warn(s"Could not find attributes object in type $unhandled")
          PhpAttributes.Empty
      }
    }
  }

  final case class PhpFile(children: Seq[PhpStmt])
  sealed abstract class PhpNode {
    def attributes: PhpAttributes
  }

  final case class PhpParam(
    name: String,
    byRef: Boolean,
    isVariadic: Boolean,
    default: Option[PhpExpr],
    // TODO type
    flags: Int,
    // TODO attributeGroups: Seq[PhpAttributeGroup],
    attributes: PhpAttributes
  ) extends PhpNode

  sealed abstract class PhpArgument extends PhpNode
  final case class PhpArg(
    expr: PhpExpr,
    parameterName: Option[String],
    byRef: Boolean,
    unpack: Boolean,
    attributes: PhpAttributes
  ) extends PhpArgument
  object PhpArg {
    def apply(expr: PhpExpr): PhpArg = {
      PhpArg(expr, parameterName = None, byRef = false, unpack = false, attributes = expr.attributes)
    }
  }
  final case class PhpVariadicPlaceholder(attributes: Domain.PhpAttributes) extends PhpArgument

  sealed abstract class PhpStmt extends PhpNode

  final case class PhpEchoStmt(exprs: Seq[PhpExpr], attributes: PhpAttributes) extends PhpStmt

  final case class PhpMethodDecl(
    name: String,
    params: Seq[PhpParam],
    // TODO returnType: Option[String],
    stmts: Seq[PhpStmt],
    returnByRef: Boolean,
    // TODO attributeGroups: Seq[PhpAttributeGroup],
    namespacedName: Option[String],
    attributes: PhpAttributes
  ) extends PhpStmt

  sealed abstract class PhpExpr extends PhpStmt

  final case class PhpFuncCall(name: PhpExpr, args: Seq[PhpArgument], attributes: PhpAttributes) extends PhpExpr
  final case class PhpVariable(value: PhpExpr, attributes: PhpAttributes)                        extends PhpExpr
  final case class PhpNameExpr(name: String, attributes: PhpAttributes)                          extends PhpExpr
  final case class PhpBinaryOp(operator: String, left: PhpExpr, right: PhpExpr, attributes: PhpAttributes)
      extends PhpExpr
  object PhpBinaryOp {
    val BinaryOpTypeMap: Map[String, String] = Map(
      "Expr_BinaryOp_BitwiseAnd"     -> Operators.and,
      "Expr_BinaryOp_BitwiseOr"      -> Operators.or,
      "Expr_BinaryOp_BitwiseXor"     -> Operators.xor,
      "Expr_BinaryOp_BooleanAnd"     -> Operators.logicalAnd,
      "Expr_BinaryOp_BooleanOr"      -> Operators.logicalOr,
      "Expr_BinaryOp_Coalesce"       -> PhpOperators.coalesce,
      "Expr_BinaryOp_Concat"         -> PhpOperators.concat,
      "Expr_BinaryOp_Div"            -> Operators.division,
      "Expr_BinaryOp_Equal"          -> Operators.equals,
      "Expr_BinaryOp_GreaterOrEqual" -> Operators.greaterEqualsThan,
      "Expr_BinaryOp_Greater"        -> Operators.greaterThan,
      "Expr_BinaryOp_Identical"      -> PhpOperators.identical,
      "Expr_BinaryOp_LogicalAnd"     -> Operators.logicalAnd,
      "Expr_BinaryOp_LogicalOr"      -> Operators.logicalOr,
      "Expr_BinaryOp_LogicalXor"     -> PhpOperators.logicalXor,
      "Expr_BinaryOp_Minus"          -> Operators.minus,
      "Expr_BinaryOp_Mod"            -> Operators.modulo,
      "Expr_BinaryOp_Mul"            -> Operators.multiplication,
      "Expr_BinaryOp_NotEqual"       -> Operators.notEquals,
      "Expr_BinaryOp_NotIdentical"   -> PhpOperators.notIdentical,
      "Expr_BinaryOp_Plus"           -> Operators.plus,
      "Expr_BinaryOp_Pow"            -> Operators.exponentiation,
      "Expr_BinaryOp_ShiftLeft"      -> Operators.shiftLeft,
      "Expr_BinaryOp_ShiftRight"     -> Operators.arithmeticShiftRight,
      "Expr_BinaryOp_SmallerOrEqual" -> Operators.lessEqualsThan,
      "Expr_BinaryOp_Smaller"        -> Operators.lessThan,
      "Expr_BinaryOp_Spaceship"      -> PhpOperators.spaceship
    )

    def isBinaryOpType(typeName: String): Boolean = {
      BinaryOpTypeMap.contains(typeName)
    }
  }
  final case class PhpUnaryOp(operator: String, expr: PhpExpr, attributes: PhpAttributes) extends PhpExpr
  object PhpUnaryOp {
    val UnaryOpTypeMap: Map[String, String] = Map(
      "Expr_BitwiseNot" -> Operators.not,
      "Expr_BooleanNot" -> Operators.logicalNot,
      "Expr_PostDec"    -> Operators.postDecrement,
      "Expr_PostInc"    -> Operators.postIncrement,
      "Expr_PreDec"     -> Operators.preDecrement,
      "Expr_PreInc"     -> Operators.preIncrement,
      "Expr_UnaryMinus" -> Operators.minus,
      "Expr_UnaryPlus"  -> Operators.plus
    )

    def isUnaryOpType(typeName: String): Boolean = {
      UnaryOpTypeMap.contains(typeName)
    }
  }

  object PhpAssignment {
    val AssignTypeMap: Map[String, String] = Map(
      "Expr_Assign"              -> Operators.assignment,
      "Expr_AssignRef"           -> Operators.assignment,
      "Expr_AssignOp_BitwiseAnd" -> Operators.assignmentAnd,
      "Expr_AssignOp_BitwiseOr"  -> Operators.assignmentOr,
      "Expr_AssignOp_BitwiseXor" -> Operators.assignmentXor,
      "Expr_AssignOp_Coalesce"   -> PhpOperators.assignmentCoalesce,
      "Expr_AssignOp_Concat"     -> PhpOperators.assignmentConcat,
      "Expr_AssignOp_Div"        -> Operators.assignmentDivision,
      "Expr_AssignOp_Minus"      -> Operators.assignmentMinus,
      "Expr_AssignOp_Mod"        -> Operators.assignmentModulo,
      "Expr_AssignOp_Mul"        -> Operators.assignmentMultiplication,
      "Expr_AssignOp_Plus"       -> Operators.assignmentPlus,
      "Expr_AssignOp_Pow"        -> Operators.assignmentExponentiation,
      "Expr_AssignOp_ShiftLeft"  -> Operators.assignmentShiftLeft,
      "Expr_AssignOp_ShiftRight" -> Operators.assignmentArithmeticShiftRight
    )

    def isAssignType(typeName: String): Boolean = {
      AssignTypeMap.contains(typeName)
    }
  }
  final case class PhpAssignment(assignOp: String, target: PhpExpr, source: PhpExpr, attributes: PhpAttributes)
      extends PhpExpr

  sealed abstract class PhpScalar                                      extends PhpExpr
  final case class PhpString(value: String, attributes: PhpAttributes) extends PhpScalar
  object PhpString {
    def withQuotes(value: String, attributes: PhpAttributes): PhpString = {
      PhpString(s"\"${escapeString(value)}\"", attributes)
    }
  }

  final case class PhpInt(value: String, attributes: PhpAttributes)            extends PhpScalar
  final case class PhpFloat(value: String, attributes: PhpAttributes)          extends PhpScalar
  final case class PhpEncapsed(parts: Seq[PhpExpr], attributes: PhpAttributes) extends PhpScalar
  final case class PhpEncapsedPart(value: String, attributes: PhpAttributes)   extends PhpScalar
  object PhpEncapsedPart {
    def withQuotes(value: String, attributes: PhpAttributes): PhpEncapsedPart = {
      PhpEncapsedPart(s"\"${escapeString(value)}\"", attributes)
    }
  }

  private def escapeString(value: String): String = {
    value
      .replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\b", "\\b")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      .replace("\'", "\\'")
      .replace("\f", "\\f")
      .replace("\"", "\\\"")
  }

  private def readFile(json: Value): PhpFile = {
    json match {
      case arr: Arr =>
        val children = arr.value.map(readStmt).toSeq
        PhpFile(children)
      case unhandled =>
        logger.error(s"Found unhandled type in readFile: ${unhandled.getClass} with value $unhandled")
        ???
    }
  }

  private def readStmt(json: Value): PhpStmt = {
    json("nodeType").str match {
      case "Stmt_Echo" =>
        val values = json("exprs").arr.map(readExpr).toSeq
        PhpEchoStmt(values, PhpAttributes(json))
      case "Stmt_Expression" => readExpr(json("expr"))
      case "Stmt_Function"   => readFunction(json)
      case "Stmt_InlineHTML" => readInlineHtml(json)
      case unhandled =>
        logger.error(s"Found unhandled stmt type: $unhandled")
        ???
    }
  }

  private def readInlineHtml(json: Value): PhpStmt = {
    val attributes = PhpAttributes(json)
    val value      = PhpString.withQuotes(json("value").str, attributes)
    PhpEchoStmt(List(value), attributes)
  }

  private def readExpr(json: Value): PhpExpr = {
    json("nodeType").str match {
      case "Scalar_String"             => PhpString.withQuotes(json("value").str, PhpAttributes(json))
      case "Scalar_DNumber"            => PhpFloat(json("value").toString, PhpAttributes(json))
      case "Scalar_LNumber"            => PhpInt(json("value").toString, PhpAttributes(json))
      case "Scalar_Encapsed"           => PhpEncapsed(json("parts").arr.map(readExpr).toSeq, PhpAttributes(json))
      case "Scalar_EncapsedStringPart" => PhpEncapsedPart.withQuotes(json("value").str, PhpAttributes(json))

      case "Expr_FuncCall" => readFunctionCall(json)
      case "Expr_Variable" => readVariable(json)

      case typ if isUnaryOpType(typ)  => readUnaryOp(json)
      case typ if isBinaryOpType(typ) => readBinaryOp(json)
      case typ if isAssignType(typ)   => readAssign(json)

      case unhandled =>
        logger.error(s"Found unhandled expr type: $unhandled")
        ???
    }
  }

  private def readVariable(json: Value): PhpVariable = {
    // TODO Figure out when the variable has an expr name
    PhpVariable(readName(json("name")), PhpAttributes(json))
  }

  private def readFunctionCall(json: Value): PhpFuncCall = {
    val args = json("args").arr.map(readCallArg).toSeq
    PhpFuncCall(readName(json("name")), args, PhpAttributes(json))
  }

  private def readFunction(json: Value): PhpMethodDecl = {
    val jsonMap = json.obj
    PhpMethodDecl(
      name = jsonMap("name")("name").str,
      params = jsonMap("params").arr.map(readParam).toSeq,
      stmts = jsonMap("stmts").arr.map(readStmt).toSeq,
      returnByRef = jsonMap("byRef").bool,
      namespacedName = jsonMap.get("namespacedName").map(_.obj).flatMap(constructNamespacedName),
      PhpAttributes(json)
    )
  }

  private def readParam(json: Value): PhpParam = {
    PhpParam(
      name = json("var")("name").str,
      byRef = json("byRef").bool,
      isVariadic = json("variadic").bool,
      default = json.obj.get("default").filterNot(_.isNull).map(readExpr),
      flags = json("flags").num.toInt,
      attributes = PhpAttributes(json)
    )
  }

  private def readName(json: Value): PhpExpr = {
    json match {
      case Str(name) => PhpNameExpr(name, PhpAttributes.Empty)

      case Obj(value) if value.get("nodeType").map(_.str).contains("Name_FullyQualified") =>
        val name = value("parts").arr.map(_.str).mkString(FullyQualifiedNameDelimiter)
        PhpNameExpr(name, PhpAttributes(json))
    }
  }

  private def readUnaryOp(json: Value): PhpUnaryOp = {
    val opType = UnaryOpTypeMap(json("nodeType").str)

    val expr =
      if (json.obj.contains("expr"))
        readExpr(json.obj("expr"))
      else if (json.obj.contains("var"))
        readVariable(json.obj("var"))
      else
        throw new UnsupportedOperationException(s"Expected expr or var field in unary op but found $json")

    PhpUnaryOp(opType, expr, PhpAttributes(json))
  }

  private def readBinaryOp(json: Value): PhpBinaryOp = {
    val opType = BinaryOpTypeMap(json("nodeType").str)

    val leftExpr  = readExpr(json("left"))
    val rightExpr = readExpr(json("right"))

    PhpBinaryOp(opType, leftExpr, rightExpr, PhpAttributes(json))
  }

  private def readAssign(json: Value): PhpAssignment = {
    val opType = AssignTypeMap(json("nodeType").str)

    val target = readExpr(json("var"))
    val source = readExpr(json("expr"))

    PhpAssignment(opType, target, source, PhpAttributes(json))
  }

  private def readCallArg(json: Value): PhpArgument = {
    json("nodeType").str match {
      case "Arg" =>
        PhpArg(
          expr = readExpr(json("value")),
          parameterName = json.obj.get("name").filterNot(_.isNull).map(_("name").str),
          byRef = json("byRef").bool,
          unpack = json("unpack").bool,
          attributes = PhpAttributes(json)
        )

      case "VariadicPlaceholder" => PhpVariadicPlaceholder(PhpAttributes(json))
    }
  }

  private def constructNamespacedName(nameObj: mutable.LinkedHashMap[String, Value]): Option[String] = {
    nameObj.value.get("parts").map(_.arr.mkString(NamespaceDelimiter))
  }

  def fromJson(jsonInput: Value): PhpFile = {
    readFile(jsonInput)
  }
}
