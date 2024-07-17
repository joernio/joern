package io.joern.php2cpg.parser

import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain.PhpAssignment.AssignTypeMap
import io.joern.php2cpg.parser.Domain.PhpAssignment.isAssignType
import io.joern.php2cpg.parser.Domain.PhpBinaryOp.BinaryOpTypeMap
import io.joern.php2cpg.parser.Domain.PhpBinaryOp.isBinaryOpType
import io.joern.php2cpg.parser.Domain.PhpCast.CastTypeMap
import io.joern.php2cpg.parser.Domain.PhpCast.isCastType
import io.joern.php2cpg.parser.Domain.PhpUnaryOp.UnaryOpTypeMap
import io.joern.php2cpg.parser.Domain.PhpUnaryOp.isUnaryOpType
import io.joern.php2cpg.parser.Domain.PhpUseType.PhpUseType
import io.joern.php2cpg.parser.Domain.PhpUseType.getUseType
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Operators
import org.slf4j.LoggerFactory
import ujson.Arr
import ujson.Obj
import ujson.Str
import ujson.Value

import scala.util.Success
import scala.util.Try

object Domain {

  object PhpOperators {
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
    // Double arrow operator used to represent key/value pairs: key => value
    val doubleArrow = "<operator>.doubleArrow"

    val assignmentCoalesceOp = "<operator>.assignmentCoalesce"
    val assignmentConcatOp   = "<operator>.assignmentConcat"

    val encaps      = "encaps"
    val declareFunc = "declare"
    val global      = "global"

    // These are handled as special cases for builtins since they have separate AST nodes in the PHP-parser output.
    val issetFunc = s"isset"
    val printFunc = s"print"
    val cloneFunc = s"clone"
    val emptyFunc = s"empty"
    val evalFunc  = s"eval"
    val exitFunc  = s"exit"
    // Used for multiple assignments for example `list($a, $b) = $someArray`
    val listFunc  = s"list"
    val isNull    = s"is_null"
    val unset     = s"unset"
    val shellExec = s"shell_exec"

    // Used for composer dependencies
    val autoload = "<autoload>"
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

  private val logger          = LoggerFactory.getLogger(Domain.getClass)
  val NamespaceDelimiter      = "\\"
  val StaticMethodDelimiter   = "::"
  val InstanceMethodDelimiter = "->"
  // Used for creating the default constructor.
  val ConstructorMethodName = "__construct"

  final case class PhpAttributes(lineNumber: Option[Int], kind: Option[Int], startFilePos: Int, endFilePos: Int)
  object PhpAttributes {
    val Empty: PhpAttributes = PhpAttributes(None, None, -1, -1)

    def apply(json: Value): PhpAttributes = {
      Try(json("attributes")) match {
        case Success(Obj(attributes)) =>
          val startLine    = attributes.get("startLine").map(_.num.toInt)
          val kind         = attributes.get("kind").map(_.num.toInt)
          val startFilePos = attributes.get("startFilePos").map(_.num.toInt).getOrElse(-1)
          val endFilePos   = attributes.get("endFilePos").map(_.num.toInt + 1).getOrElse(-1)
          PhpAttributes(startLine, kind, startFilePos, endFilePos)

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

    private val AccessModifiers: Set[String] = Set(ModifierTypes.PUBLIC, ModifierTypes.PROTECTED, ModifierTypes.PRIVATE)

    def containsAccessModifier(modifiers: List[String]): Boolean = {
      modifiers.toSet.intersect(AccessModifiers).nonEmpty
    }

    def getModifierSet(json: Value, modifierString: String = "flags"): List[String] = {
      val flags = json.objOpt.flatMap(_.get(modifierString)).map(_.num.toInt).getOrElse(0)
      ModifierMasks.collect {
        case (mask, typ) if (flags & mask) != 0 => typ
      }
    }
  }

  sealed trait PhpNode {
    def attributes: PhpAttributes
  }

  final case class PhpFile(children: List[PhpStmt]) extends PhpNode {
    override val attributes: PhpAttributes = PhpAttributes.Empty
  }

  final case class PhpParam(
    name: String,
    paramType: Option[PhpNameExpr],
    byRef: Boolean,
    isVariadic: Boolean,
    default: Option[PhpExpr],
    // TODO type
    flags: Int,
    attributes: PhpAttributes,
    attributeGroups: Seq[PhpAttributeGroup]
  ) extends PhpNode

  sealed trait PhpArgument extends PhpNode
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

  sealed trait PhpStmt extends PhpNode
  sealed trait PhpStmtWithBody extends PhpStmt {
    def stmts: List[PhpStmt]
  }

  // In the PhpParser output, comments are included as an attribute to the first statement following the comment. If
  // no such statement exists, a Nop statement (which does not exist in PHP) is added as a sort of comment container.
  final case class NopStmt(attributes: PhpAttributes)                                           extends PhpStmt
  final case class PhpEchoStmt(exprs: Seq[PhpExpr], attributes: PhpAttributes)                  extends PhpStmt
  final case class PhpBreakStmt(num: Option[Int], attributes: PhpAttributes)                    extends PhpStmt
  final case class PhpContinueStmt(num: Option[Int], attributes: PhpAttributes)                 extends PhpStmt
  final case class PhpWhileStmt(cond: PhpExpr, stmts: List[PhpStmt], attributes: PhpAttributes) extends PhpStmtWithBody
  final case class PhpDoStmt(cond: PhpExpr, stmts: List[PhpStmt], attributes: PhpAttributes)    extends PhpStmtWithBody
  final case class PhpForStmt(
    inits: List[PhpExpr],
    conditions: List[PhpExpr],
    loopExprs: List[PhpExpr],
    stmts: List[PhpStmt],
    attributes: PhpAttributes
  ) extends PhpStmtWithBody
  final case class PhpIfStmt(
    cond: PhpExpr,
    stmts: List[PhpStmt],
    elseIfs: List[PhpElseIfStmt],
    elseStmt: Option[PhpElseStmt],
    attributes: PhpAttributes
  ) extends PhpStmtWithBody
  final case class PhpElseIfStmt(cond: PhpExpr, stmts: List[PhpStmt], attributes: PhpAttributes) extends PhpStmtWithBody
  final case class PhpElseStmt(stmts: List[PhpStmt], attributes: PhpAttributes)                  extends PhpStmtWithBody
  final case class PhpSwitchStmt(condition: PhpExpr, cases: List[PhpCaseStmt], attributes: PhpAttributes)
      extends PhpStmt
  final case class PhpCaseStmt(condition: Option[PhpExpr], stmts: List[PhpStmt], attributes: PhpAttributes)
      extends PhpStmtWithBody
  final case class PhpTryStmt(
    stmts: List[PhpStmt],
    catches: List[PhpCatchStmt],
    finallyStmt: Option[PhpFinallyStmt],
    attributes: PhpAttributes
  ) extends PhpStmtWithBody
  final case class PhpCatchStmt(
    types: List[PhpNameExpr],
    variable: Option[PhpExpr],
    stmts: List[PhpStmt],
    attributes: PhpAttributes
  ) extends PhpStmtWithBody
  final case class PhpFinallyStmt(stmts: List[PhpStmt], attributes: PhpAttributes) extends PhpStmtWithBody
  final case class PhpReturnStmt(expr: Option[PhpExpr], attributes: PhpAttributes) extends PhpStmt

  final case class PhpMethodDecl(
    name: PhpNameExpr,
    params: Seq[PhpParam],
    modifiers: List[String],
    returnType: Option[PhpNameExpr],
    stmts: List[PhpStmt],
    returnByRef: Boolean,
    namespacedName: Option[PhpNameExpr],
    isClassMethod: Boolean,
    attributes: PhpAttributes,
    attributeGroups: Seq[PhpAttributeGroup]
  ) extends PhpStmtWithBody

  final case class PhpAttributeGroup(attrs: List[PhpAttribute], attributes: PhpAttributes)

  final case class PhpAttribute(name: PhpNameExpr, args: List[PhpArgument], attributes: PhpAttributes) extends PhpExpr

  final case class PhpClassLikeStmt(
    name: Option[PhpNameExpr],
    modifiers: List[String],
    extendsNames: List[PhpNameExpr],
    implementedInterfaces: List[PhpNameExpr],
    stmts: List[PhpStmt],
    classLikeType: String,
    // Optionally used for enums with values
    scalarType: Option[PhpNameExpr],
    hasConstructor: Boolean,
    attributes: PhpAttributes,
    attributeGroups: Seq[PhpAttributeGroup]
  ) extends PhpStmtWithBody
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
      extends PhpStmtWithBody

  final case class PhpDeclareStmt(
    declares: Seq[PhpDeclareItem],
    stmts: Option[List[PhpStmt]],
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpDeclareItem(key: PhpNameExpr, value: PhpExpr, attributes: PhpAttributes) extends PhpStmt

  final case class PhpUnsetStmt(vars: List[PhpExpr], attributes: PhpAttributes) extends PhpStmt

  final case class PhpStaticStmt(vars: List[PhpStaticVar], attributes: PhpAttributes) extends PhpStmt

  final case class PhpStaticVar(variable: PhpVariable, defaultValue: Option[PhpExpr], attributes: PhpAttributes)
      extends PhpStmt

  final case class PhpGlobalStmt(vars: List[PhpExpr], attributes: PhpAttributes) extends PhpStmt

  final case class PhpUseStmt(uses: List[PhpUseUse], useType: PhpUseType, attributes: PhpAttributes) extends PhpStmt
  final case class PhpGroupUseStmt(
    prefix: PhpNameExpr,
    uses: List[PhpUseUse],
    useType: PhpUseType,
    attributes: PhpAttributes
  ) extends PhpStmt
  final case class PhpUseUse(
    originalName: PhpNameExpr,
    alias: Option[PhpNameExpr],
    useType: PhpUseType,
    attributes: PhpAttributes
  ) extends PhpStmt

  case object PhpUseType {
    sealed trait PhpUseType
    case object Unknown  extends PhpUseType
    case object Normal   extends PhpUseType
    case object Function extends PhpUseType
    case object Constant extends PhpUseType

    def getUseType(typeNum: Int): PhpUseType = {
      typeNum match {
        case 1 => Normal
        case 2 => Function
        case 3 => Constant
        case _ => Unknown
      }
    }
  }

  final case class PhpForeachStmt(
    iterExpr: PhpExpr,
    keyVar: Option[PhpExpr],
    valueVar: PhpExpr,
    assignByRef: Boolean,
    stmts: List[PhpStmt],
    attributes: PhpAttributes
  ) extends PhpStmtWithBody
  final case class PhpTraitUseStmt(
    traits: List[PhpNameExpr],
    adaptations: List[PhpTraitUseAdaptation],
    attributes: PhpAttributes
  ) extends PhpStmt
  sealed trait PhpTraitUseAdaptation extends PhpStmt
  final case class PhpPrecedenceAdaptation(
    traitName: PhpNameExpr,
    methodName: PhpNameExpr,
    insteadOf: List[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpTraitUseAdaptation
  final case class PhpAliasAdaptation(
    traitName: Option[PhpNameExpr],
    methodName: PhpNameExpr,
    newModifier: Option[String],
    newName: Option[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpTraitUseAdaptation

  sealed trait PhpExpr extends PhpStmt

  final case class PhpNewExpr(className: PhpNode, args: List[PhpArgument], attributes: PhpAttributes) extends PhpExpr

  final case class PhpIncludeExpr(expr: PhpExpr, includeType: String, attributes: PhpAttributes) extends PhpExpr
  case object PhpIncludeType {
    val Include: String     = "include"
    val IncludeOnce: String = "include_once"
    val Require: String     = "require"
    val RequireOnce: String = "require_once"
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
      "Expr_BinaryOp_Coalesce"       -> PhpOperators.coalesceOp,
      "Expr_BinaryOp_Concat"         -> PhpOperators.concatOp,
      "Expr_BinaryOp_Div"            -> Operators.division,
      "Expr_BinaryOp_Equal"          -> Operators.equals,
      "Expr_BinaryOp_GreaterOrEqual" -> Operators.greaterEqualsThan,
      "Expr_BinaryOp_Greater"        -> Operators.greaterThan,
      "Expr_BinaryOp_Identical"      -> PhpOperators.identicalOp,
      "Expr_BinaryOp_LogicalAnd"     -> Operators.logicalAnd,
      "Expr_BinaryOp_LogicalOr"      -> Operators.logicalOr,
      "Expr_BinaryOp_LogicalXor"     -> PhpOperators.logicalXorOp,
      "Expr_BinaryOp_Minus"          -> Operators.minus,
      "Expr_BinaryOp_Mod"            -> Operators.modulo,
      "Expr_BinaryOp_Mul"            -> Operators.multiplication,
      "Expr_BinaryOp_NotEqual"       -> Operators.notEquals,
      "Expr_BinaryOp_NotIdentical"   -> PhpOperators.notIdenticalOp,
      "Expr_BinaryOp_Plus"           -> Operators.plus,
      "Expr_BinaryOp_Pow"            -> Operators.exponentiation,
      "Expr_BinaryOp_ShiftLeft"      -> Operators.shiftLeft,
      "Expr_BinaryOp_ShiftRight"     -> Operators.arithmeticShiftRight,
      "Expr_BinaryOp_SmallerOrEqual" -> Operators.lessEqualsThan,
      "Expr_BinaryOp_Smaller"        -> Operators.lessThan,
      "Expr_BinaryOp_Spaceship"      -> PhpOperators.spaceshipOp
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
      "Expr_AssignOp_Coalesce"   -> PhpOperators.assignmentCoalesceOp,
      "Expr_AssignOp_Concat"     -> PhpOperators.assignmentConcatOp,
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

  sealed trait PhpScalar extends PhpExpr
  sealed abstract class PhpSimpleScalar(val typeFullName: String) extends PhpScalar {
    def value: String
    def attributes: PhpAttributes
  }

  final case class PhpString(val value: String, val attributes: PhpAttributes)
      extends PhpSimpleScalar(TypeConstants.String)
  object PhpString {
    def withQuotes(value: String, attributes: PhpAttributes): PhpString = {
      PhpString(s"\"${escapeString(value)}\"", attributes)
    }
  }

  final case class PhpInt(val value: String, val attributes: PhpAttributes) extends PhpSimpleScalar(TypeConstants.Int)

  final case class PhpFloat(val value: String, val attributes: PhpAttributes)
      extends PhpSimpleScalar(TypeConstants.Float)

  final case class PhpEncapsed(parts: Seq[PhpExpr], attributes: PhpAttributes) extends PhpScalar

  final case class PhpThrowExpr(expr: PhpExpr, attributes: PhpAttributes)                    extends PhpExpr
  final case class PhpListExpr(items: List[Option[PhpArrayItem]], attributes: PhpAttributes) extends PhpExpr

  final case class PhpClassConstFetchExpr(
    className: PhpExpr,
    constantName: Option[PhpNameExpr],
    attributes: PhpAttributes
  ) extends PhpExpr

  final case class PhpConstFetchExpr(name: PhpNameExpr, attributes: PhpAttributes) extends PhpExpr

  final case class PhpArrayExpr(items: List[Option[PhpArrayItem]], attributes: PhpAttributes) extends PhpExpr
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

  final case class PhpShellExecExpr(parts: PhpEncapsed, attributes: PhpAttributes) extends PhpExpr

  final case class PhpPropertyFetchExpr(
    expr: PhpExpr,
    name: PhpExpr,
    isNullsafe: Boolean,
    isStatic: Boolean,
    attributes: PhpAttributes
  ) extends PhpExpr

  final case class PhpMatchExpr(condition: PhpExpr, matchArms: List[PhpMatchArm], attributes: PhpAttributes)
      extends PhpExpr

  final case class PhpMatchArm(conditions: List[PhpExpr], body: PhpExpr, isDefault: Boolean, attributes: PhpAttributes)
      extends PhpExpr

  final case class PhpYieldExpr(key: Option[PhpExpr], value: Option[PhpExpr], attributes: PhpAttributes) extends PhpExpr
  final case class PhpYieldFromExpr(expr: PhpExpr, attributes: PhpAttributes)                            extends PhpExpr

  final case class PhpClosureExpr(
    params: List[PhpParam],
    stmts: List[PhpStmt],
    returnType: Option[PhpNameExpr],
    uses: List[PhpClosureUse],
    isStatic: Boolean,
    returnByRef: Boolean,
    isArrowFunc: Boolean,
    attributes: PhpAttributes
  ) extends PhpExpr
  final case class PhpClosureUse(variable: PhpExpr, byRef: Boolean, attributes: PhpAttributes) extends PhpExpr

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
        val children = arr.value.map(readStmt).toList
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
      case "Stmt_Unset"        => readUnset(json)
      case "Stmt_Static"       => readStatic(json)
      case "Stmt_Global"       => readGlobal(json)
      case "Stmt_Use"          => readUse(json)
      case "Stmt_GroupUse"     => readGroupUse(json)
      case "Stmt_Foreach"      => readForeach(json)
      case "Stmt_TraitUse"     => readTraitUse(json)
      case unhandled =>
        logger.error(s"Found unhandled stmt type: $unhandled")
        ???
    }
  }

  private def readString(json: Value): PhpString = {
    PhpString.withQuotes(json("value").str, PhpAttributes(json))
  }

  private def readInlineHtml(json: Value): PhpStmt = {
    val value = readString(json)
    PhpEchoStmt(List(value), value.attributes)
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
      case 1 => PhpIncludeType.Include
      case 2 => PhpIncludeType.IncludeOnce
      case 3 => PhpIncludeType.Require
      case 4 => PhpIncludeType.RequireOnce
      case other =>
        logger.warn(s"Unhandled include type: $other. Defaulting to regular include.")
        PhpIncludeType.Include
    }

    PhpIncludeExpr(expr, includeType, PhpAttributes(json))
  }

  private def readMatch(json: Value): PhpMatchExpr = {
    val condition = readExpr(json("cond"))
    val matchArms = json("arms").arr.map(readMatchArm).toList

    PhpMatchExpr(condition, matchArms, PhpAttributes(json))
  }

  private def readMatchArm(json: Value): PhpMatchArm = {
    val conditions = json("conds") match {
      case ujson.Null => Nil
      case conds      => conds.arr.map(readExpr).toList
    }

    val isDefault = json("conds").isNull
    val body      = readExpr(json("body"))

    PhpMatchArm(conditions, body, isDefault, PhpAttributes(json))
  }

  private def readYield(json: Value): PhpYieldExpr = {
    val key   = Option.unless(json("key").isNull)(readExpr(json("key")))
    val value = Option.unless(json("value").isNull)(readExpr(json("value")))

    PhpYieldExpr(key, value, PhpAttributes(json))
  }

  private def readYieldFrom(json: Value): PhpYieldFromExpr = {
    val expr = readExpr(json("expr"))

    PhpYieldFromExpr(expr, PhpAttributes(json))
  }

  private def readClosure(json: Value): PhpClosureExpr = {
    val params      = json("params").arr.map(readParam).toList
    val stmts       = json("stmts").arr.map(readStmt).toList
    val returnType  = Option.unless(json("returnType").isNull)(readType(json("returnType")))
    val uses        = json("uses").arr.map(readClosureUse).toList
    val isStatic    = json("static").bool
    val isByRef     = json("byRef").bool
    val isArrowFunc = false

    PhpClosureExpr(params, stmts, returnType, uses, isStatic, isByRef, isArrowFunc, PhpAttributes(json))
  }

  private def readClosureUse(json: Value): PhpClosureUse = {
    val variable = readVariable(json("var"))
    val isByRef  = json("byRef").bool

    PhpClosureUse(variable, isByRef, PhpAttributes(json))
  }

  private def readClassConstFetch(json: Value): PhpClassConstFetchExpr = {
    val classNameType = json("class")("nodeType").str
    val className =
      if (classNameType.startsWith("Name"))
        readName(json("class"))
      else
        readExpr(json("class"))

    val constantName = json("name") match {
      case str: Str => Some(PhpNameExpr(str.value, PhpAttributes(json)))
      case obj: Obj if obj("nodeType").strOpt.contains("Expr_Error") => None
      case obj: Obj                                                  => Some(readName(obj))
      case other => throw new NotImplementedError(s"unexpected constant name '$other' of type ${other.getClass}")
    }

    PhpClassConstFetchExpr(className, constantName, PhpAttributes(json))
  }

  private def readConstFetch(json: Value): PhpConstFetchExpr = {
    val name = readName(json("name"))

    PhpConstFetchExpr(name, PhpAttributes(json))
  }

  private def readArray(json: Value): PhpArrayExpr = {
    val items = json("items").arr.map { item =>
      Option.unless(item.isNull)(readArrayItem(item))
    }.toList
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
    val parts = readEncapsed(json)

    PhpShellExecExpr(parts, PhpAttributes(json))
  }

  private def readArrowFunction(json: Value): PhpClosureExpr = {
    val params      = json("params").arr.map(readParam).toList
    val expr        = readExpr(json("expr"))
    val returnType  = Option.unless(json("returnType").isNull)(readType(json("returnType")))
    val isStatic    = json("static").bool
    val returnByRef = json("byRef").bool
    val uses        = Nil // Not defined for arrow shorthand
    val isArrowFunc = true

    // Introduce a return here to keep arrow functions consistent with regular closures while allowing easy code re-use.
    val syntheticReturn = PhpReturnStmt(Some(expr), expr.attributes)
    PhpClosureExpr(
      params,
      syntheticReturn :: Nil,
      returnType,
      uses,
      isStatic,
      returnByRef,
      isArrowFunc,
      PhpAttributes(json)
    )
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
        case other => throw new NotImplementedError(s"unexpected 'extends' entry '$other' of type ${other.getClass}")
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

    val hasConstructor = classLikeType == ClassLikeTypes.Class

    val attributes = PhpAttributes(json)

    val attributeGroups = json("attrGroups").arr.map(readAttributeGroup).toList

    PhpClassLikeStmt(
      name,
      modifiers,
      extendsNames,
      implements,
      stmts,
      classLikeType,
      scalarType,
      hasConstructor,
      attributes,
      attributeGroups
    )
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

  private def readMagicConst(json: Value): PhpConstFetchExpr = {
    val name = json("nodeType").str match {
      case "Scalar_MagicConst_Class"     => "__CLASS__"
      case "Scalar_MagicConst_Dir"       => "__DIR__"
      case "Scalar_MagicConst_File"      => "__FILE__"
      case "Scalar_MagicConst_Function"  => "__FUNCTION__"
      case "Scalar_MagicConst_Line"      => "__LINE__"
      case "Scalar_MagicConst_Method"    => "__METHOD__"
      case "Scalar_MagicConst_Namespace" => "__NAMESPACE__"
      case "Scalar_MagicConst_Trait"     => "__TRAIT__"
    }

    val attributes = PhpAttributes(json)

    PhpConstFetchExpr(PhpNameExpr(name, attributes), attributes)
  }

  private def readExpr(json: Value): PhpExpr = {
    json("nodeType").str match {
      case "Scalar_String"             => readString(json)
      case "Scalar_DNumber"            => PhpFloat(json("value").toString, PhpAttributes(json))
      case "Scalar_LNumber"            => PhpInt(json("value").toString, PhpAttributes(json))
      case "Scalar_Encapsed"           => readEncapsed(json)
      case "Scalar_InterpolatedString" => readEncapsed(json)
      case "Scalar_EncapsedStringPart" => readString(json)
      case "InterpolatedStringPart"    => readString(json)

      case typ if typ.startsWith("Scalar_MagicConst") => readMagicConst(json)

      case "Expr_FuncCall"           => readCall(json)
      case "Expr_MethodCall"         => readCall(json)
      case "Expr_NullsafeMethodCall" => readCall(json)
      case "Expr_StaticCall"         => readCall(json)

      case "Expr_Clone"     => readClone(json)
      case "Expr_Empty"     => readEmpty(json)
      case "Expr_Eval"      => readEval(json)
      case "Expr_Exit"      => readExit(json)
      case "Expr_Variable"  => readVariable(json)
      case "Expr_Isset"     => readIsset(json)
      case "Expr_Print"     => readPrint(json)
      case "Expr_Ternary"   => readTernaryOp(json)
      case "Expr_Throw"     => readThrow(json)
      case "Expr_List"      => readList(json)
      case "Expr_New"       => readNew(json)
      case "Expr_Include"   => readInclude(json)
      case "Expr_Match"     => readMatch(json)
      case "Expr_Yield"     => readYield(json)
      case "Expr_YieldFrom" => readYieldFrom(json)
      case "Expr_Closure"   => readClosure(json)

      case "Expr_ClassConstFetch" => readClassConstFetch(json)
      case "Expr_ConstFetch"      => readConstFetch(json)

      case "Expr_Array"         => readArray(json)
      case "Expr_ArrayDimFetch" => readArrayDimFetch(json)
      case "Expr_ErrorSuppress" => readErrorSuppress(json)
      case "Expr_Instanceof"    => readInstanceOf(json)
      case "Expr_ShellExec"     => readShellExec(json)
      case "Expr_ArrowFunction" => readArrowFunction(json)

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
    if (!json.obj.contains("name")) {
      logger.error(s"Variable did not contain name: $json")
    }
    val varAttrs = PhpAttributes(json)
    val name = json("name") match {
      case Str(value) => readName(value).copy(attributes = varAttrs)
      case Obj(_)     => readNameOrExpr(json, "name")
      case value      => readExpr(value)
    }
    PhpVariable(name, varAttrs)
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
    if (field("nodeType").str.startsWith("Name"))
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
    val returnType  = Option.unless(json("returnType").isNull)(readType(json("returnType")))
    val stmts       = json("stmts").arr.map(readStmt).toList
    // Only class methods have modifiers
    val modifiers       = Nil
    val namespacedName  = Option.unless(json("namespacedName").isNull)(readName(json("namespacedName")))
    val isClassMethod   = false
    val attributeGroups = json("attrGroups").arr.map(readAttributeGroup).toSeq

    PhpMethodDecl(
      name,
      params,
      modifiers,
      returnType,
      stmts,
      returnByRef,
      namespacedName,
      isClassMethod,
      PhpAttributes(json),
      attributeGroups
    )
  }

  private def readClassMethod(json: Value): PhpMethodDecl = {
    val modifiers   = PhpModifiers.getModifierSet(json)
    val returnByRef = json("byRef").bool
    val name        = readName(json("name"))
    val params      = json("params").arr.map(readParam).toList
    val returnType  = Option.unless(json("returnType").isNull)(readType(json("returnType")))
    val stmts =
      if (json("stmts").isNull)
        Nil
      else
        json("stmts").arr.map(readStmt).toList

    val namespacedName  = None // only defined for functions
    val isClassMethod   = true
    val attributeGroups = json("attrGroups").arr.map(readAttributeGroup).toSeq

    PhpMethodDecl(
      name,
      params,
      modifiers,
      returnType,
      stmts,
      returnByRef,
      namespacedName,
      isClassMethod,
      PhpAttributes(json),
      attributeGroups
    )
  }

  private def readAttributeGroup(json: Value): PhpAttributeGroup = {
    PhpAttributeGroup(json("attrs").arr.map(readAttribute).toList, PhpAttributes(json))
  }

  private def readAttribute(json: Value): PhpAttribute = {
    PhpAttribute(readName(json("name")), json("args").arr.map(readCallArg).toList, PhpAttributes(json))
  }

  private def readProperty(json: Value): PhpPropertyStmt = {
    val modifiers = PhpModifiers.getModifierSet(json)
    val variables = json("props").arr.map(readPropertyValue).toList
    val typeName  = Option.unless(json("type").isNull)(readType(json("type")))

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

  private def readUnset(json: Value): PhpUnsetStmt = {
    val vars = json("vars").arr.map(readExpr).toList

    PhpUnsetStmt(vars, PhpAttributes(json))
  }

  private def readStatic(json: Value): PhpStaticStmt = {
    val vars = json("vars").arr.map(readStaticVar).toList

    PhpStaticStmt(vars, PhpAttributes(json))
  }

  private def readGlobal(json: Value): PhpGlobalStmt = {
    val vars = json("vars").arr.map(readExpr).toList

    PhpGlobalStmt(vars, PhpAttributes(json))
  }

  private def readUse(json: Value): PhpUseStmt = {
    val useType = getUseType(json("type").num.toInt)
    val uses    = json("uses").arr.map(readUseUse(_, useType)).toList

    PhpUseStmt(uses, useType, PhpAttributes(json))
  }

  private def readGroupUse(json: Value): PhpGroupUseStmt = {
    val prefix  = readName(json("prefix"))
    val useType = getUseType(json("type").num.toInt)
    val uses    = json("uses").arr.map(readUseUse(_, useType)).toList

    PhpGroupUseStmt(prefix, uses, useType, PhpAttributes(json))
  }

  private def readForeach(json: Value): PhpForeachStmt = {
    val iterExpr    = readExpr(json("expr"))
    val keyVar      = Option.unless(json("keyVar").isNull)(readExpr(json("keyVar")))
    val valueVar    = readExpr(json("valueVar"))
    val assignByRef = json("byRef").bool
    val stmts       = json("stmts").arr.map(readStmt).toList

    PhpForeachStmt(iterExpr, keyVar, valueVar, assignByRef, stmts, PhpAttributes(json))
  }

  private def readTraitUse(json: Value): PhpTraitUseStmt = {
    val traits      = json("traits").arr.map(readName).toList
    val adaptations = json("adaptations").arr.map(readTraitUseAdaptation).toList
    PhpTraitUseStmt(traits, adaptations, PhpAttributes(json))
  }

  private def readTraitUseAdaptation(json: Value): PhpTraitUseAdaptation = {
    json("nodeType").str match {
      case "Stmt_TraitUseAdaptation_Alias"      => readAliasAdaptation(json)
      case "Stmt_TraitUseAdaptation_Precedence" => readPrecedenceAdaptation(json)
    }
  }

  private def readAliasAdaptation(json: Value): PhpAliasAdaptation = {
    val traitName  = Option.unless(json("trait").isNull)(readName(json("trait")))
    val methodName = readName(json("method"))
    val newName    = Option.unless(json("newName").isNull)(readName(json("newName")))

    val newModifier = json("newModifier") match {
      case ujson.Null => None
      case _          => PhpModifiers.getModifierSet(json, "newModifier").headOption
    }
    PhpAliasAdaptation(traitName, methodName, newModifier, newName, PhpAttributes(json))
  }

  private def readPrecedenceAdaptation(json: Value): PhpPrecedenceAdaptation = {
    val traitName  = readName(json("trait"))
    val methodName = readName(json("method"))
    val insteadOf  = json("insteadof").arr.map(readName).toList

    PhpPrecedenceAdaptation(traitName, methodName, insteadOf, PhpAttributes(json))
  }

  private def readUseUse(json: Value, parentType: PhpUseType): PhpUseUse = {
    val name  = readName(json("name"))
    val alias = Option.unless(json("alias").isNull)(readName(json("alias")))
    val useType =
      if (parentType == PhpUseType.Unknown)
        getUseType(json("type").num.toInt)
      else
        parentType

    PhpUseUse(name, alias, useType, PhpAttributes(json))
  }

  private def readStaticVar(json: Value): PhpStaticVar = {
    val variable     = readVariable(json("var"))
    val defaultValue = Option.unless(json("default").isNull)(readExpr(json("default")))

    PhpStaticVar(variable, defaultValue, PhpAttributes(json))
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
    val paramType       = Option.unless(json("type").isNull)(readType(json("type")))
    val attributeGroups = json("attrGroups").arr.map(readAttributeGroup).toSeq
    PhpParam(
      name = json("var")("name").str,
      paramType = paramType,
      byRef = json("byRef").bool,
      isVariadic = json("variadic").bool,
      default = json.obj.get("default").filterNot(_.isNull).map(readExpr),
      flags = json("flags").num.toInt,
      attributes = PhpAttributes(json),
      attributeGroups
    )
  }

  private def readName(json: Value): PhpNameExpr = {
    json match {
      case Str(name) => PhpNameExpr(name, PhpAttributes.Empty)

      case Obj(value) if value.get("nodeType").map(_.str).contains("Name_FullyQualified") =>
        val name = value("parts").arr.map(_.str).mkString(NamespaceDelimiter)
        PhpNameExpr(name, PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("Name") =>
        // TODO Can this case just be merged with Name_FullyQualified?
        val name = value("parts").arr.map(_.str).mkString(NamespaceDelimiter)
        PhpNameExpr(name, PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("Identifier") =>
        val name = value("name").str
        PhpNameExpr(name, PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("VarLikeIdentifier") =>
        val name = value("name").str
        PhpNameExpr(name, PhpAttributes(json))

      case unhandled =>
        logger.error(s"Found unhandled name type $unhandled: $json")
        ??? // TODO: other matches are possible?
    }
  }

  /** One of Identifier, Name, or Complex Type (Nullable, Intersection, or Union)
    */
  private def readType(json: Value): PhpNameExpr = {
    json match {
      case Obj(value) if value.get("nodeType").map(_.str).contains("NullableType") =>
        val containedName = readType(value("type")).name
        PhpNameExpr(s"?$containedName", attributes = PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("IntersectionType") =>
        val names = value("types").arr.map(readName).map(_.name)
        PhpNameExpr(names.mkString("&"), PhpAttributes(json))

      case Obj(value) if value.get("nodeType").map(_.str).contains("UnionType") =>
        val names = value("types").arr.map(readType).map(_.name)
        PhpNameExpr(names.mkString("|"), PhpAttributes(json))

      case other => readName(other)
    }
  }

  private def readUnaryOp(json: Value): PhpUnaryOp = {
    val opType = UnaryOpTypeMap(json("nodeType").str)

    val expr =
      if (json.obj.contains("expr"))
        readExpr(json.obj("expr"))
      else if (json.obj.contains("var"))
        readExpr(json.obj("var"))
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

  def fromJson(jsonInput: Value): PhpFile = {
    readFile(jsonInput)
  }
}
