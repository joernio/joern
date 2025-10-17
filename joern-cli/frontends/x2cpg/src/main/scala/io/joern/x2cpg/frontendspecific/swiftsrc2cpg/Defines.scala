package io.joern.x2cpg.frontendspecific.swiftsrc2cpg

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}

object Defines {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val Any: String                    = "ANY"
  val Character: String              = "Swift.Character"
  val String: String                 = "Swift.String"
  val Int: String                    = "Swift.Int"
  val Float: String                  = "Swift.Float"
  val Double: String                 = "Swift.Double"
  val Bool: String                   = "Swift.Bool"
  val Function: String               = "Swift.Function"
  val Array: String                  = "Swift.Array"
  val Dictionary: String             = "Swift.Dictionary"
  val Nil: String                    = "Swift.Nil"
  val Iterator: String               = "Swift.Iterator"
  val Void: String                   = "()"
  val ConstructorMethodName: String  = "init"
  val ClosureApplyMethodName: String = "single_apply"
  val DuplicateSuffix: String        = "<duplicate>"
  val GlobalNamespace: String        = NamespaceTraversal.globalNamespaceName

  val SwiftTypes: List[String] =
    List(Any, Nil, Character, String, Int, Float, Double, Bool, Function, Array, Dictionary, Iterator, Void)

  val PostfixOperatorMap: Map[String, String] = Map(
    "++"  -> Operators.postIncrement,
    "--"  -> Operators.postDecrement,
    "^"   -> Operators.xor,
    "<"   -> Operators.lessThan,
    ">"   -> Operators.greaterThan,
    "..." -> "<operator>.splat"
  ).withDefault { key =>
    logger.info(s"Postfix operator '$key' not handled yet")
    key
  }

  val PrefixOperatorMap: Map[String, String] = Map(
    "-"   -> Operators.preDecrement,
    "+"   -> Operators.preIncrement,
    "~"   -> Operators.not,
    "!"   -> Operators.logicalNot,
    "..<" -> Operators.lessThan,
    "<"   -> Operators.lessThan,
    "..>" -> Operators.greaterThan,
    ">"   -> Operators.greaterThan,
    "=="  -> Operators.equals,
    "%"   -> Operators.modulo,
    "&"   -> Operators.addressOf,
    "&+"  -> Operators.plus,
    "&-"  -> Operators.minus,
    "..." -> "<operator>.splat"
  ).withDefault { key =>
    logger.info(s"Prefix operator '$key' not handled yet")
    key
  }

  val InfixOperatorMap: Map[String, String] = Map(
    "="    -> Operators.assignment,
    "+="   -> Operators.assignmentPlus,
    "-="   -> Operators.assignmentMinus,
    "*="   -> Operators.assignmentMultiplication,
    "/="   -> Operators.assignmentDivision,
    "%="   -> Operators.assignmentModulo,
    "**="  -> Operators.assignmentExponentiation,
    "&&"   -> Operators.logicalAnd,
    "&"    -> Operators.and,
    "&="   -> Operators.assignmentAnd,
    "&&="  -> Operators.assignmentAnd,
    "|="   -> Operators.assignmentOr,
    "|"    -> Operators.or,
    "||="  -> Operators.assignmentOr,
    "||"   -> Operators.logicalOr,
    "^="   -> Operators.assignmentXor,
    "&<<"  -> Operators.shiftLeft,
    "<<"   -> Operators.shiftLeft,
    "&>>=" -> Operators.assignmentArithmeticShiftRight,
    "&>>"  -> Operators.arithmeticShiftRight,
    ">>"   -> Operators.arithmeticShiftRight,
    "&<<=" -> Operators.assignmentShiftLeft,
    "<<="  -> Operators.assignmentShiftLeft,
    ">>="  -> Operators.assignmentArithmeticShiftRight,
    ">>>=" -> Operators.assignmentLogicalShiftRight,
    "??="  -> Operators.notNullAssert,
    "<="   -> Operators.lessEqualsThan,
    ">="   -> Operators.greaterEqualsThan,
    "<"    -> Operators.lessThan,
    ">"    -> Operators.greaterThan,
    "=="   -> Operators.equals,
    "!="   -> Operators.notEquals,
    "==="  -> Operators.equals,
    "!=="  -> Operators.notEquals,
    "&+"   -> Operators.addition,
    "+"    -> Operators.addition,
    "&+="  -> Operators.assignmentPlus,
    "&-"   -> Operators.subtraction,
    "-"    -> Operators.subtraction,
    "&-="  -> Operators.assignmentMinus,
    "&/"   -> Operators.division,
    "/"    -> Operators.division,
    "&/="  -> Operators.assignmentDivision,
    "&*"   -> Operators.multiplication,
    "*"    -> Operators.multiplication,
    "&*="  -> Operators.assignmentMultiplication,
    "..<"  -> Operators.range,
    ">.."  -> Operators.range,
    "..."  -> Operators.range,
    "%"    -> Operators.modulo,
    "!"    -> Operators.logicalNot,
    "^"    -> Operators.xor,
    "??"   -> Operators.elvis,
    "~="   -> Operators.in
  ).withDefault { key =>
    logger.info(s"Infix operator '$key' not handled yet")
    key
  }
}
