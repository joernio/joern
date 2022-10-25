package io.joern.php2cpg.parser

import io.joern.php2cpg.parser.Domain.PhpAssignment.{AssignTypeMap, isAssignType}
import io.joern.php2cpg.parser.Domain.PhpBinaryOp.{BinaryOpTypeMap, isBinaryOpType}
import io.joern.php2cpg.parser.Domain.PhpCast.{CastTypeMap, isCastType}
import io.joern.php2cpg.parser.Domain.PhpUnaryOp.{UnaryOpTypeMap, isUnaryOpType}
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import org.slf4j.LoggerFactory
import ujson.{Arr, Obj, Str, Value}

import scala.collection.mutable
import scala.util.{Success, Try}

object Domain {

  object PhpBuiltins {
    // TODO Decide which of these should be moved to codepropertygraph
    val coalesceOp     = "<operator>.coalesce"
    val concatOp       = "<operator>.concat"
    val identicalOp    = "<operator>.identical"
    val logicalXorOp   = "<operator>.logicalXor"
    val notIdenticalOp = "<operator>.notIdentical"
    val spaceshipOp    = "<operator>.spaceship"
    val elvisOp        = "<operator>.elvis"
    val unpack         = "<operator>.unpack"
    // Used for $array[] = $var type assignments
    val emptyArrayIdx = "<operator>.emptyArrayIdx"
    val errorSuppress = "<operator>.errorSuppress"

    val assignmentCoalesceOp = "<operator>.assignmentCoalesce"
    val assignmentConcatOp   = "<operator>.assignmentConcat"

    val encaps    = "encaps"
    val issetFunc = "isset"
    val printFunc = "print"
    val cloneFunc = "clone"
    val emptyFunc = "empty"
    val evalFunc  = "eval"
    val exitFunc  = "exit"
    // Used for multiple assignments for example `list($a, $b) = $someArray`
    val listFunc    = "list"
    val declareFunc = "declare"
    val shellExec   = "shell_exec"
  }

  object PhpDomainTypeConstants {
    val array  = "array"
    val bool   = "bool"
    val double = "double"
    val int    = "int"
    val obj    = "object"
    val string = "string"
    val unset  = "unset"
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

        case Success(Arr(_)) =>
          logger.debug(s"Found array attributes in $json")
          PhpAttributes.Empty

        case unhandled =>
          logger.warn(s"Could not find attributes object in type $unhandled")
          PhpAttributes.Empty
      }
    }
  }

  object PhpModifiers {
    private val ModifierMasks = List(
      (1, ModifierTypes.PUBLIC),
      (2, ModifierTypes.PROTECTED),
      (4, ModifierTypes.PRIVATE),
      (8, ModifierTypes.STATIC),
      (16, ModifierTypes.ABSTRACT),
      (32, ModifierTypes.FINAL),
      (64, ModifierTypes.READONLY)
    )

    def getModifierSet(json: Value): List[String] = {
      val flags = json.objOpt.flatMap(_.get("flags")).map(_.num.toInt).getOrElse(0)
      ModifierMasks.collect {
        case (mask, typ) if (flags & mask) != 0 => typ
      }
    }
  }

  final case class PhpFile(children: Seq[PhpStmt])
  sealed abstract class PhpNode {
    def attributes: PhpAttributes
  }

  final case class PhpParam(
    name: String,
    paramType: Option[PhpNameExpr],
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

  // In the PhpParser output, comments are included as an attribute to the first statement following the comment. If
  // no such statement exists, a Nop statement (which does not exist in PHP) is added as a sort of comment container.
  final case class NopStmt(attributes: PhpAttributes)                                           extends PhpStmt
  final case class PhpEchoStmt(exprs: Seq[PhpExpr], attributes: PhpAttributes)                  extends PhpStmt
  final case class PhpBreakStmt(num: Option[Int], attributes: PhpAttributes)                    extends PhpStmt
  final case class PhpContinueStmt(num: Option[Int], attributes: PhpAttributes)                 extends PhpStmt
  final case class PhpWhileStmt(cond: PhpExpr, stmts: List[PhpStmt], attributes: PhpAttributes) extends PhpStmt
  final case class PhpDoStmt(cond: PhpExpr, stmts: List[PhpStmt], attributes: PhpAttributes)    extends PhpStmt
  final case class PhpForStmt(
    inits: List[PhpExpr],
    conditions: List[PhpExpr],
    loopExprs: List[PhpExpr],
    bodyStmts: List[PhpStmt],
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpIfStmt(
    cond: PhpExpr,
    stmts: List[PhpStmt],
    elseIfs: List[PhpElseIfStmt],
    elseStmt: Option[PhpElseStmt],
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpElseIfStmt(cond: PhpExpr, stmts: List[PhpStmt], attributes: PhpAttributes) extends PhpStmt
  final case class PhpElseStmt(stmts: List[PhpStmt], attributes: PhpAttributes)                  extends PhpStmt
  final case class PhpSwitchStmt(condition: PhpExpr, cases: List[PhpCaseStmt], attributes: PhpAttributes)
      extends PhpStmt
  final case class PhpCaseStmt(condition: Option[PhpExpr], stmts: List[PhpStmt], attributes: PhpAttributes)
      extends PhpStmt
  final case class PhpTryStmt(
    stmts: List[PhpStmt],
    catches: List[PhpCatchStmt],
    finallyStmt: Option[PhpFinallyStmt],
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpCatchStmt(
    types: List[PhpNameExpr],
    variable: Option[PhpExpr],
    stmts: List[PhpStmt],
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpFinallyStmt(stmts: List[PhpStmt], attributes: PhpAttributes) extends PhpStmt
  final case class PhpReturnStmt(expr: Option[PhpExpr], attributes: PhpAttributes) extends PhpStmt

  final case class PhpMethodDecl(
    name: PhpNameExpr,
    params: Seq[PhpParam],
    modifiers: List[String],
    returnType: Option[PhpNameExpr],
    stmts: Seq[PhpStmt],
    returnByRef: Boolean,
    // TODO attributeGroups: Seq[PhpAttributeGroup],
    namespacedName: Option[PhpNameExpr],
    isClassMethod: Boolean,
    attributes: PhpAttributes
  ) extends PhpStmt

  final case class PhpClassLikeStmt(
    name: Option[PhpNameExpr],
    modifiers: List[String],
    extendsNames: List[PhpNameExpr],
    implementedInterfaces: List[PhpNameExpr],
    stmts: List[PhpStmt],
    classLikeType: String,
    // Optionally used for enums with values
    scalarType: Option[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpStmt
  object ClassLikeTypes {
    val Class: String     = "class"
    val Trait: String     = "trait"
    val Interface: String = "interface"
    val Enum: String      = "enum"
  }

  final case class PhpEnumCaseStmt(name: PhpNameExpr, expr: Option[PhpExpr], attributes: PhpAttributes) extends PhpStmt

  final case class PhpPropertyStmt(
    modifiers: List[String],
    variables: List[PhpPropertyValue],
    typeName: Option[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpStmt

  final case class PhpPropertyValue(name: PhpNameExpr, defaultValue: Option[PhpExpr], attributes: PhpAttributes)
      extends PhpStmt

  final case class PhpConstStmt(modifiers: List[String], consts: List[PhpConstDeclaration], attributes: PhpAttributes)
      extends PhpStmt

  final case class PhpGotoStmt(label: PhpNameExpr, attributes: PhpAttributes)  extends PhpStmt
  final case class PhpLabelStmt(label: PhpNameExpr, attributes: PhpAttributes) extends PhpStmt
  final case class PhpHaltCompilerStmt(attributes: PhpAttributes)              extends PhpStmt

  final case class PhpConstDeclaration(
    name: PhpNameExpr,
    value: PhpExpr,
    namespacedName: Option[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpStmt

  final case class PhpNamespaceStmt(name: Option[PhpNameExpr], stmts: List[PhpStmt], attributes: PhpAttributes)
      extends PhpStmt

  final case class PhpDeclareStmt(
    declares: Seq[PhpDeclareItem],
    stmts: Option[List[PhpStmt]],
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpDeclareItem(key: PhpNameExpr, value: PhpExpr, attributes: PhpAttributes) extends PhpStmt

  sealed abstract class PhpExpr extends PhpStmt

  final case class PhpNewExpr(className: PhpNode, args: List[PhpArgument], attributes: PhpAttributes) extends PhpExpr

  final case class PhpIncludeExpr(expr: PhpExpr, includeType: String, attributes: PhpAttributes) extends PhpExpr
  case object PhpIncludeType {
    val include: String     = "include"
    val includeOnce: String = "include_once"
    val require: String     = "require"
    val requireOnce: String = "require_once"
  }

  final case class PhpCallExpr(
    target: Option[PhpExpr],
    methodName: PhpExpr,
    args: Seq[PhpArgument],
    isNullSafe: Boolean,
    isStatic: Boolean,
    attributes: PhpAttributes
  ) extends PhpExpr
  final case class PhpVariable(value: PhpExpr, attributes: PhpAttributes)        extends PhpExpr
  final case class PhpNameExpr(name: String, attributes: PhpAttributes)          extends PhpExpr
  final case class PhpCloneExpr(expr: PhpExpr, attributes: PhpAttributes)        extends PhpExpr
  final case class PhpEmptyExpr(expr: PhpExpr, attributes: PhpAttributes)        extends PhpExpr
  final case class PhpEvalExpr(expr: PhpExpr, attributes: PhpAttributes)         extends PhpExpr
  final case class PhpExitExpr(expr: Option[PhpExpr], attributes: PhpAttributes) extends PhpExpr
  final case class PhpBinaryOp(operator: String, left: PhpExpr, right: PhpExpr, attributes: PhpAttributes)
      extends PhpExpr
  object PhpBinaryOp {
    val BinaryOpTypeMap: Map[String, String] = Map(
      "Expr_BinaryOp_BitwiseAnd"     -> Operators.and,
      "Expr_BinaryOp_BitwiseOr"      -> Operators.or,
      "Expr_BinaryOp_BitwiseXor"     -> Operators.xor,
      "Expr_BinaryOp_BooleanAnd"     -> Operators.logicalAnd,
      "Expr_BinaryOp_BooleanOr"      -> Operators.logicalOr,
      "Expr_BinaryOp_Coalesce"       -> PhpBuiltins.coalesceOp,
      "Expr_BinaryOp_Concat"         -> PhpBuiltins.concatOp,
      "Expr_BinaryOp_Div"            -> Operators.division,
      "Expr_BinaryOp_Equal"          -> Operators.equals,
      "Expr_BinaryOp_GreaterOrEqual" -> Operators.greaterEqualsThan,
      "Expr_BinaryOp_Greater"        -> Operators.greaterThan,
      "Expr_BinaryOp_Identical"      -> PhpBuiltins.identicalOp,
      "Expr_BinaryOp_LogicalAnd"     -> Operators.logicalAnd,
      "Expr_BinaryOp_LogicalOr"      -> Operators.logicalOr,
      "Expr_BinaryOp_LogicalXor"     -> PhpBuiltins.logicalXorOp,
      "Expr_BinaryOp_Minus"          -> Operators.minus,
      "Expr_BinaryOp_Mod"            -> Operators.modulo,
      "Expr_BinaryOp_Mul"            -> Operators.multiplication,
      "Expr_BinaryOp_NotEqual"       -> Operators.notEquals,
      "Expr_BinaryOp_NotIdentical"   -> PhpBuiltins.notIdenticalOp,
      "Expr_BinaryOp_Plus"           -> Operators.plus,
      "Expr_BinaryOp_Pow"            -> Operators.exponentiation,
      "Expr_BinaryOp_ShiftLeft"      -> Operators.shiftLeft,
      "Expr_BinaryOp_ShiftRight"     -> Operators.arithmeticShiftRight,
      "Expr_BinaryOp_SmallerOrEqual" -> Operators.lessEqualsThan,
      "Expr_BinaryOp_Smaller"        -> Operators.lessThan,
      "Expr_BinaryOp_Spaceship"      -> PhpBuiltins.spaceshipOp
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
  final case class PhpTernaryOp(
    condition: PhpExpr,
    thenExpr: Option[PhpExpr],
    elseExpr: PhpExpr,
    attributes: PhpAttributes
  ) extends PhpExpr

  object PhpAssignment {
    val AssignTypeMap: Map[String, String] = Map(
      "Expr_Assign"              -> Operators.assignment,
      "Expr_AssignRef"           -> Operators.assignment,
      "Expr_AssignOp_BitwiseAnd" -> Operators.assignmentAnd,
      "Expr_AssignOp_BitwiseOr"  -> Operators.assignmentOr,
      "Expr_AssignOp_BitwiseXor" -> Operators.assignmentXor,
      "Expr_AssignOp_Coalesce"   -> PhpBuiltins.assignmentCoalesceOp,
      "Expr_AssignOp_Concat"     -> PhpBuiltins.assignmentConcatOp,
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
  final case class PhpAssignment(
    assignOp: String,
    target: PhpExpr,
    source: PhpExpr,
    isRefAssign: Boolean,
    attributes: PhpAttributes
  ) extends PhpExpr

  final case class PhpCast(typ: String, expr: PhpExpr, attributes: PhpAttributes) extends PhpExpr
  object PhpCast {
    val CastTypeMap: Map[String, String] = Map(
      "Expr_Cast_Array"  -> PhpDomainTypeConstants.array,
      "Expr_Cast_Bool"   -> PhpDomainTypeConstants.bool,
      "Expr_Cast_Double" -> PhpDomainTypeConstants.double,
      "Expr_Cast_Int"    -> PhpDomainTypeConstants.int,
      "Expr_Cast_Object" -> PhpDomainTypeConstants.obj,
      "Expr_Cast_String" -> PhpDomainTypeConstants.string,
      "Expr_Cast_Unset"  -> PhpDomainTypeConstants.unset
    )

    def isCastType(typeName: String): Boolean = {
      CastTypeMap.contains(typeName)
    }
  }

  final case class PhpIsset(vars: Seq[PhpExpr], attributes: PhpAttributes) extends PhpExpr
  final case class PhpPrint(expr: PhpExpr, attributes: PhpAttributes)      extends PhpExpr

  sealed abstract class PhpScalar extends PhpExpr
  sealed abstract class PhpSimpleScalar extends PhpScalar {
    def value: String
    def attributes: PhpAttributes
  }
  final case class PhpString(value: String, attributes: PhpAttributes) extends PhpSimpleScalar
  object PhpString {
    def withQuotes(value: String, attributes: PhpAttributes): PhpString = {
      PhpString(s"\"${escapeString(value)}\"", attributes)
    }
  }

  final case class PhpInt(value: String, attributes: PhpAttributes)            extends PhpSimpleScalar
  final case class PhpFloat(value: String, attributes: PhpAttributes)          extends PhpSimpleScalar
  final case class PhpEncapsed(parts: Seq[PhpExpr], attributes: PhpAttributes) extends PhpScalar
  final case class PhpEncapsedPart(value: String, attributes: PhpAttributes)   extends PhpScalar
  object PhpEncapsedPart {
    def withQuotes(value: String, attributes: PhpAttributes): PhpEncapsedPart = {
      PhpEncapsedPart(s"\"${escapeString(value)}\"", attributes)
    }
  }
  final case class PhpThrowExpr(expr: PhpExpr, attributes: PhpAttributes)                    extends PhpExpr
  final case class PhpListExpr(items: List[Option[PhpArrayItem]], attributes: PhpAttributes) extends PhpExpr

  final case class PhpClassConstFetchExpr(
    className: PhpExpr,
    constantName: Option[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpExpr

  final case class PhpConstFetchExpr(name: PhpNameExpr, attributes: PhpAttributes) extends PhpExpr

  final case class PhpArrayExpr(items: List[PhpArrayItem], attributes: PhpAttributes) extends PhpExpr
  final case class PhpArrayItem(
    key: Option[PhpExpr],
    value: PhpExpr,
    byRef: Boolean,
    unpack: Boolean,
    attributes: PhpAttributes
  ) extends PhpExpr
  final case class PhpArrayDimFetchExpr(variable: PhpExpr, dimension: Option[PhpExpr], attributes: PhpAttributes)
      extends PhpExpr

  final case class PhpErrorSuppressExpr(expr: PhpExpr, attributes: PhpAttributes) extends PhpExpr

  final case class PhpInstanceOfExpr(expr: PhpExpr, className: PhpExpr, attributes: PhpAttributes) extends PhpExpr

  final case class PhpShellExecExpr(parts: List[PhpExpr], attributes: PhpAttributes) extends PhpExpr

  final case class PhpPropertyFetchExpr(
    expr: PhpExpr,
    name: PhpExpr,
    isNullsafe: Boolean,
    isStatic: Boolean,
    attributes: PhpAttributes
  ) extends PhpExpr

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
      case "Stmt_Expression"   => readExpr(json("expr"))
      case "Stmt_Function"     => readFunction(json)
      case "Stmt_InlineHTML"   => readInlineHtml(json)
      case "Stmt_Break"        => readBreak(json)
      case "Stmt_Continue"     => readContinue(json)
      case "Stmt_While"        => readWhile(json)
      case "Stmt_Do"           => readDo(json)
      case "Stmt_For"          => readFor(json)
      case "Stmt_If"           => readIf(json)
      case "Stmt_Switch"       => readSwitch(json)
      case "Stmt_TryCatch"     => readTry(json)
      case "Stmt_Throw"        => readThrow(json)
      case "Stmt_Return"       => readReturn(json)
      case "Stmt_Class"        => readClassLike(json, ClassLikeTypes.Class)
      case "Stmt_Interface"    => readClassLike(json, ClassLikeTypes.Interface)
      case "Stmt_Trait"        => readClassLike(json, ClassLikeTypes.Trait)
      case "Stmt_Enum"         => readClassLike(json, ClassLikeTypes.Enum)
      case "Stmt_EnumCase"     => readEnumCase(json)
      case "Stmt_ClassMethod"  => readClassMethod(json)
      case "Stmt_Property"     => readProperty(json)
      case "Stmt_ClassConst"   => readConst(json)
      case "Stmt_Const"        => readConst(json)
      case "Stmt_Goto"         => readGoto(json)
      case "Stmt_Label"        => readLabel(json)
      case "Stmt_HaltCompiler" => readHaltCompiler(json)
      case "Stmt_Namespace"    => readNamespace(json)
      case "Stmt_Nop"          => NopStmt(PhpAttributes(json))
      case "Stmt_Declare"      => readDeclare(json)
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

  private def readBreakContinueNum(json: Value): Option[Int] = {
    Option.unless(json("num").isNull)(json("num")("value").toString).flatMap(_.toIntOption)
  }
  private def readBreak(json: Value): PhpBreakStmt = {
    val num = readBreakContinueNum(json)
    PhpBreakStmt(num, PhpAttributes(json))
  }

  private def readContinue(json: Value): PhpContinueStmt = {
    val num = readBreakContinueNum(json)
    PhpContinueStmt(num, PhpAttributes(json))
  }

  private def readWhile(json: Value): PhpWhileStmt = {
    val cond  = readExpr(json("cond"))
    val stmts = json("stmts").arr.toList.map(readStmt)
    PhpWhileStmt(cond, stmts, PhpAttributes(json))
  }

  private def readDo(json: Value): PhpDoStmt = {
    val cond  = readExpr(json("cond"))
    val stmts = json("stmts").arr.toList.map(readStmt)
    PhpDoStmt(cond, stmts, PhpAttributes(json))
  }

  private def readFor(json: Value): PhpForStmt = {
    val inits      = json("init").arr.map(readExpr).toList
    val conditions = json("cond").arr.map(readExpr).toList
    val loopExprs  = json("loop").arr.map(readExpr).toList
    val bodyStmts  = json("stmts").arr.map(readStmt).toList

    PhpForStmt(inits, conditions, loopExprs, bodyStmts, PhpAttributes(json))
  }

  private def readIf(json: Value): PhpIfStmt = {
    val condition = readExpr(json("cond"))
    val stmts     = json("stmts").arr.map(readStmt).toList
    val elseIfs   = json("elseifs").arr.map(readElseIf).toList
    val elseStmt  = Option.when(!json("else").isNull)(readElse(json("else")))

    PhpIfStmt(condition, stmts, elseIfs, elseStmt, PhpAttributes(json))
  }

  private def readSwitch(json: Value): PhpSwitchStmt = {
    val condition = readExpr(json("cond"))
    val cases     = json("cases").arr.map(readCase).toList

    PhpSwitchStmt(condition, cases, PhpAttributes(json))
  }

  private def readTry(json: Value): PhpTryStmt = {
    val stmts       = json("stmts").arr.map(readStmt).toList
    val catches     = json("catches").arr.map(readCatch).toList
    val finallyStmt = Option.unless(json("finally").isNull)(readFinally(json("finally")))

    PhpTryStmt(stmts, catches, finallyStmt, PhpAttributes(json))
  }

  private def readThrow(json: Value): PhpThrowExpr = {
    val expr = readExpr(json("expr"))

    PhpThrowExpr(expr, PhpAttributes(json))
  }

  private def readList(json: Value): PhpListExpr = {
    val items = json("items").arr.map(item => Option.unless(item.isNull)(readArrayItem(item))).toList

    PhpListExpr(items, PhpAttributes(json))
  }

  private def readNew(json: Value): PhpNewExpr = {
    val classNode =
      if (json("class")("nodeType").strOpt.contains("Stmt_Class"))
        readClassLike(json("class"), ClassLikeTypes.Class)
      else
        readNameOrExpr(json, "class")

    val args = json("args").arr.map(readCallArg).toList

    PhpNewExpr(classNode, args, PhpAttributes(json))
  }

  private def readInclude(json: Value): PhpIncludeExpr = {
    val expr = readExpr(json("expr"))
    val includeType = json("type").num.toInt match {
      case 1 => PhpIncludeType.include
      case 2 => PhpIncludeType.includeOnce
      case 3 => PhpIncludeType.require
      case 4 => PhpIncludeType.requireOnce
      case other =>
        logger.warn(s"Unhandled include type: $other. Defaulting to regular include.")
        PhpIncludeType.include
    }

    PhpIncludeExpr(expr, includeType, PhpAttributes(json))
  }

  private def readClassConstFetch(json: Value): PhpClassConstFetchExpr = {
    val classNameType = json("class")("nodeType").str
    val className =
      if (classNameType.startsWith("Name_"))
        readName(json("class"))
      else
        readExpr(json("class"))

    val constantName = json("name") match {
      case str: Str => Some(PhpNameExpr(str.value, PhpAttributes(json)))

      case obj: Obj if obj("nodeType").strOpt.contains("Expr_Error") => None

      case obj: Obj => Some(readName(obj))
    }

    PhpClassConstFetchExpr(className, constantName, PhpAttributes(json))
  }

  private def readConstFetch(json: Value): PhpConstFetchExpr = {
    val name = readName(json("name"))

    PhpConstFetchExpr(name, PhpAttributes(json))
  }

  private def readArray(json: Value): PhpArrayExpr = {
    val items = json("items").arr.map(readArrayItem).toList
    PhpArrayExpr(items, PhpAttributes(json))
  }

  private def readArrayItem(json: Value): PhpArrayItem = {
    val key    = Option.unless(json("key").isNull)(readExpr(json("key")))
    val value  = readExpr(json("value"))
    val byRef  = json("byRef").bool
    val unpack = json("byRef").bool

    PhpArrayItem(key, value, byRef, unpack, PhpAttributes(json))
  }

  private def readArrayDimFetch(json: Value): PhpArrayDimFetchExpr = {
    val variable  = readExpr(json("var"))
    val dimension = Option.unless(json("dim").isNull)(readExpr(json("dim")))

    PhpArrayDimFetchExpr(variable, dimension, PhpAttributes(json))
  }

  private def readErrorSuppress(json: Value): PhpErrorSuppressExpr = {
    val expr = readExpr(json("expr"))
    PhpErrorSuppressExpr(expr, PhpAttributes(json))
  }

  private def readInstanceOf(json: Value): PhpInstanceOfExpr = {
    val expr      = readExpr(json("expr"))
    val className = readNameOrExpr(json, "class")

    PhpInstanceOfExpr(expr, className, PhpAttributes(json))
  }

  private def readShellExec(json: Value): PhpShellExecExpr = {
    val parts = json("parts").arr.map(readExpr).toList

    PhpShellExecExpr(parts, PhpAttributes(json))
  }

  private def readPropertyFetch(
    json: Value,
    isNullsafe: Boolean = false,
    isStatic: Boolean = false
  ): PhpPropertyFetchExpr = {
    val expr =
      if (json.obj.contains("var"))
        readExpr(json("var"))
      else
        readNameOrExpr(json, "class")

    val name = readNameOrExpr(json, "name")

    PhpPropertyFetchExpr(expr, name, isNullsafe, isStatic, PhpAttributes(json))
  }

  private def readReturn(json: Value): PhpReturnStmt = {
    val expr = Option.unless(json("expr").isNull)(readExpr(json("expr")))

    PhpReturnStmt(expr, PhpAttributes(json))
  }

  private def extendsForClassLike(json: Value): List[PhpNameExpr] = {
    json.obj
      .get("extends")
      .map {
        case ujson.Null     => Nil
        case arr: ujson.Arr => arr.arr.map(readName).toList
        case obj: ujson.Obj => readName(obj) :: Nil
      }
      .getOrElse(Nil)
  }

  private def readClassLike(json: Value, classLikeType: String): PhpClassLikeStmt = {
    val name      = Option.unless(json("name").isNull)(readName(json("name")))
    val modifiers = PhpModifiers.getModifierSet(json)

    val extendsNames = extendsForClassLike(json)

    val implements = json.obj.get("implements").map(_.arr.toList).getOrElse(Nil).map(readName)
    val stmts      = json("stmts").arr.map(readStmt).toList

    val scalarType = json.obj.get("scalarType").flatMap(typ => Option.unless(typ.isNull)(readName(typ)))

    val attributes = PhpAttributes(json)

    PhpClassLikeStmt(name, modifiers, extendsNames, implements, stmts, classLikeType, scalarType, attributes)
  }

  private def readEnumCase(json: Value): PhpEnumCaseStmt = {
    val name = readName(json("name"))
    val expr = Option.unless(json("expr").isNull)(readExpr(json("expr")))

    PhpEnumCaseStmt(name, expr, PhpAttributes(json))
  }

  private def readCatch(json: Value): PhpCatchStmt = {
    val types    = json("types").arr.map(readName).toList
    val variable = Option.unless(json("var").isNull)(readExpr(json("var")))
    val stmts    = json("stmts").arr.map(readStmt).toList

    PhpCatchStmt(types, variable, stmts, PhpAttributes(json))
  }

  private def readFinally(json: Value): PhpFinallyStmt = {
    val stmts = json("stmts").arr.map(readStmt).toList

    PhpFinallyStmt(stmts, PhpAttributes(json))
  }

  private def readCase(json: Value): PhpCaseStmt = {
    val condition = Option.unless(json("cond").isNull)(readExpr(json("cond")))
    val stmts     = json("stmts").arr.map(readStmt).toList

    PhpCaseStmt(condition, stmts, PhpAttributes(json))
  }

  private def readElseIf(json: Value): PhpElseIfStmt = {
    val condition = readExpr(json("cond"))
    val stmts     = json("stmts").arr.map(readStmt).toList

    PhpElseIfStmt(condition, stmts, PhpAttributes(json))
  }

  private def readElse(json: Value): PhpElseStmt = {
    val stmts = json("stmts").arr.map(readStmt).toList

    PhpElseStmt(stmts, PhpAttributes(json))
  }

  private def readEncapsed(json: Value): PhpEncapsed = {
    PhpEncapsed(json("parts").arr.map(readExpr).toSeq, PhpAttributes(json))
  }

  private def readEncapsedPart(json: Value): PhpEncapsedPart = {
    PhpEncapsedPart.withQuotes(json("value").str, PhpAttributes(json))
  }

  private def readExpr(json: Value): PhpExpr = {
    json("nodeType").str match {
      case "Scalar_String"             => PhpString.withQuotes(json("value").str, PhpAttributes(json))
      case "Scalar_DNumber"            => PhpFloat(json("value").toString, PhpAttributes(json))
      case "Scalar_LNumber"            => PhpInt(json("value").toString, PhpAttributes(json))
      case "Scalar_Encapsed"           => readEncapsed(json)
      case "Scalar_InterpolatedString" => readEncapsed(json)
      case "Scalar_EncapsedStringPart" => readEncapsedPart(json)
      case "InterpolatedStringPart"    => readEncapsedPart(json)

      case "Expr_FuncCall"           => readCall(json)
      case "Expr_MethodCall"         => readCall(json)
      case "Expr_NullsafeMethodCall" => readCall(json)
      case "Expr_StaticCall"         => readCall(json)

      case "Expr_Clone"    => readClone(json)
      case "Expr_Empty"    => readEmpty(json)
      case "Expr_Eval"     => readEval(json)
      case "Expr_Exit"     => readExit(json)
      case "Expr_Variable" => readVariable(json)
      case "Expr_Isset"    => readIsset(json)
      case "Expr_Print"    => readPrint(json)
      case "Expr_Ternary"  => readTernaryOp(json)
      case "Expr_Throw"    => readThrow(json)
      case "Expr_List"     => readList(json)
      case "Expr_New"      => readNew(json)
      case "Expr_Include"  => readInclude(json)

      case "Expr_ClassConstFetch" => readClassConstFetch(json)
      case "Expr_ConstFetch"      => readConstFetch(json)

      case "Expr_Array"         => readArray(json)
      case "Expr_ArrayDimFetch" => readArrayDimFetch(json)
      case "Expr_ErrorSuppress" => readErrorSuppress(json)
      case "Expr_Instanceof"    => readInstanceOf(json)
      case "Expr_ShellExec"     => readShellExec(json)

      case "Expr_PropertyFetch"         => readPropertyFetch(json)
      case "Expr_NullsafePropertyFetch" => readPropertyFetch(json, isNullsafe = true)
      case "Expr_StaticPropertyFetch"   => readPropertyFetch(json, isStatic = true)

      case typ if isUnaryOpType(typ)  => readUnaryOp(json)
      case typ if isBinaryOpType(typ) => readBinaryOp(json)
      case typ if isAssignType(typ)   => readAssign(json)
      case typ if isCastType(typ)     => readCast(json)

      case unhandled =>
        logger.error(s"Found unhandled expr type: $unhandled")
        ???
    }
  }

  private def readClone(json: Value): PhpCloneExpr = {
    val expr = readExpr(json("expr"))
    PhpCloneExpr(expr, PhpAttributes(json))
  }

  private def readEmpty(json: Value): PhpEmptyExpr = {
    val expr = readExpr(json("expr"))
    PhpEmptyExpr(expr, PhpAttributes(json))
  }

  private def readEval(json: Value): PhpEvalExpr = {
    val expr = readExpr(json("expr"))
    PhpEvalExpr(expr, PhpAttributes(json))
  }

  private def readExit(json: Value): PhpExitExpr = {
    val expr = Option.unless(json("expr").isNull)(readExpr(json("expr")))
    PhpExitExpr(expr, PhpAttributes(json))
  }

  private def readVariable(json: Value): PhpVariable = {
    val name = json("name") match {
      case Str(value) => readName(value)
      case Obj(_)     => readNameOrExpr(json, "name")
      case value      => readExpr(value)
    }
    PhpVariable(name, PhpAttributes(json))
  }

  private def readIsset(json: Value): PhpIsset = {
    val vars = json("vars").arr.map(readExpr).toList
    PhpIsset(vars, PhpAttributes(json))
  }

  private def readPrint(json: Value): PhpPrint = {
    val expr = readExpr(json("expr"))
    PhpPrint(expr, PhpAttributes(json))
  }

  private def readTernaryOp(json: Value): PhpTernaryOp = {
    val condition     = readExpr(json("cond"))
    val maybeThenExpr = Option.unless(json("if").isNull)(readExpr(json("if")))
    val elseExpr      = readExpr(json("else"))

    PhpTernaryOp(condition, maybeThenExpr, elseExpr, PhpAttributes(json))
  }

  private def readNameOrExpr(json: Value, fieldName: String): PhpExpr = {
    val field = json(fieldName)
    if (field("nodeType").str.startsWith("Name_"))
      readName(field)
    else if (field("nodeType").str == "Identifier")
      readName(field)
    else if (field("nodeType").str == "VarLikeIdentifier") {
      readVariable(field)
    } else
      readExpr(field)
  }

  private def readCall(json: Value): PhpCallExpr = {
    val jsonMap  = json.obj
    val nodeType = json("nodeType").str
    val args     = json("args").arr.map(readCallArg).toSeq

    val target =
      jsonMap.get("var").map(readExpr).orElse(jsonMap.get("class").map(_ => readNameOrExpr(jsonMap, "class")))

    val methodName = readNameOrExpr(json, "name")

    val isNullSafe = nodeType == "Expr_NullsafeMethodCall"
    val isStatic   = nodeType == "Expr_StaticCall"

    PhpCallExpr(target, methodName, args, isNullSafe, isStatic, PhpAttributes(json))
  }

  private def readFunction(json: Value): PhpMethodDecl = {
    val returnByRef = json("byRef").bool
    val name        = readName(json("name"))
    val params      = json("params").arr.map(readParam).toList
    val returnType  = Option.unless(json("returnType").isNull)(readName(json("returnType")))
    val stmts       = json("stmts").arr.map(readStmt).toList
    // Only class methods have modifiers
    val modifiers      = Nil
    val namespacedName = Option.unless(json("namespacedName").isNull)(readName(json("namespacedName")))
    val isClassMethod  = false

    PhpMethodDecl(
      name,
      params,
      modifiers,
      returnType,
      stmts,
      returnByRef,
      namespacedName,
      isClassMethod,
      PhpAttributes(json)
    )
  }

  private def readClassMethod(json: Value): PhpMethodDecl = {
    val modifiers   = PhpModifiers.getModifierSet(json)
    val returnByRef = json("byRef").bool
    val name        = readName(json("name"))
    val params      = json("params").arr.map(readParam).toList
    val returnType  = Option.unless(json("returnType").isNull)(readName(json("returnType")))
    val stmts =
      if (json("stmts").isNull)
        Nil
      else
        json("stmts").arr.map(readStmt).toList

    val namespacedName = None // only defined for functions
    val isClassMethod  = true

    PhpMethodDecl(
      name,
      params,
      modifiers,
      returnType,
      stmts,
      returnByRef,
      namespacedName,
      isClassMethod,
      PhpAttributes(json)
    )
  }

  private def readProperty(json: Value): PhpPropertyStmt = {
    val modifiers = PhpModifiers.getModifierSet(json)
    val variables = json("props").arr.map(readPropertyValue).toList
    val typeName  = Option.unless(json("type").isNull)(readName(json("type")))

    PhpPropertyStmt(modifiers, variables, typeName, PhpAttributes(json))
  }

  private def readPropertyValue(json: Value): PhpPropertyValue = {
    val name         = readName(json("name"))
    val defaultValue = Option.unless(json("default").isNull)(readExpr(json("default")))

    PhpPropertyValue(name, defaultValue, PhpAttributes(json))
  }

  private def readConst(json: Value): PhpConstStmt = {
    val modifiers = PhpModifiers.getModifierSet(json)

    val constDeclarations = json("consts").arr.map(readConstDeclaration).toList

    PhpConstStmt(modifiers, constDeclarations, PhpAttributes(json))
  }

  private def readGoto(json: Value): PhpGotoStmt = {
    val name = readName(json("name"))
    PhpGotoStmt(name, PhpAttributes(json))
  }

  private def readLabel(json: Value): PhpLabelStmt = {
    val name = readName(json("name"))
    PhpLabelStmt(name, PhpAttributes(json))
  }

  private def readHaltCompiler(json: Value): PhpHaltCompilerStmt = {
    // Ignore the remaining text here since it can get quite large (common use case is to separate code from data blob)
    PhpHaltCompilerStmt(PhpAttributes(json))
  }

  private def readNamespace(json: Value): PhpNamespaceStmt = {
    val name = Option.unless(json("name").isNull)(readName(json("name")))

    val stmts = json("stmts") match {
      case ujson.Null => Nil
      case stmts: Arr => stmts.arr.map(readStmt).toList
      case unhandled =>
        logger.warn(s"Unhandled namespace stmts type $unhandled")
        ???
    }

    PhpNamespaceStmt(name, stmts, PhpAttributes(json))
  }

  private def readDeclare(json: Value): PhpDeclareStmt = {
    val declares = json("declares").arr.map(readDeclareItem).toList
    val stmts    = Option.unless(json("stmts").isNull)(json("stmts").arr.map(readStmt).toList)

    PhpDeclareStmt(declares, stmts, PhpAttributes(json))
  }

  private def readDeclareItem(json: Value): PhpDeclareItem = {
    val key   = readName(json("key"))
    val value = readExpr(json("value"))

    PhpDeclareItem(key, value, PhpAttributes(json))
  }

  private def readConstDeclaration(json: Value): PhpConstDeclaration = {
    val name           = readName(json("name"))
    val value          = readExpr(json("value"))
    val namespacedName = Option.unless(json("namespacedName").isNull)(readName(json("namespacedName")))

    PhpConstDeclaration(name, value, namespacedName, PhpAttributes(json))
  }

  private def readParam(json: Value): PhpParam = {
    val paramType = Option.unless(json("type").isNull)(readName(json("type")))
    PhpParam(
      name = json("var")("name").str,
      paramType = paramType,
      byRef = json("byRef").bool,
      isVariadic = json("variadic").bool,
      default = json.obj.get("default").filterNot(_.isNull).map(readExpr),
      flags = json("flags").num.toInt,
      attributes = PhpAttributes(json)
    )
  }

  private def correctConstructor(originalName: String): String = {
    originalName.replaceAll("__construct", Defines.ConstructorMethodName)
  }

  private def readName(json: Value): PhpNameExpr = {
    json match {
      case Str(name) => PhpNameExpr(correctConstructor(name), PhpAttributes.Empty)

      case Obj(value) if value.get("nodeType").map(_.str).contains("Name_FullyQualified") =>
        val name = value("parts").arr.map(_.str).mkString(FullyQualifiedNameDelimiter)
        PhpNameExpr(correctConstructor(name), PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("Name") =>
        // TODO Can this case just be merged with Name_FullyQualified?
        val name = value("parts").arr.map(_.str).mkString(FullyQualifiedNameDelimiter)
        PhpNameExpr(correctConstructor(name), PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("Identifier") =>
        val name = value("name").str
        PhpNameExpr(correctConstructor(name), PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("VarLikeIdentifier") =>
        val name = value("name").str
        PhpNameExpr(correctConstructor(name), PhpAttributes(json))

      case unhandled =>
        logger.error(s"Found unhandled name type $unhandled")
        ??? // TODO: other matches are possible?
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
    val nodeType = json("nodeType").str
    val opType   = AssignTypeMap(nodeType)

    val target = readExpr(json("var"))
    val source = readExpr(json("expr"))

    val isRefAssign = nodeType == "Expr_AssignRef"

    PhpAssignment(opType, target, source, isRefAssign, PhpAttributes(json))
  }

  private def readCast(json: Value): PhpCast = {
    val typ  = CastTypeMap(json("nodeType").str)
    val expr = readExpr(json("expr"))

    PhpCast(typ, expr, PhpAttributes(json))
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
