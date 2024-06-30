package io.joern.x2cpg.frontendspecific.swiftsrc2cpg

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}

object Defines {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val Any: String             = "ANY"
  val Character: String       = "Character"
  val String: String          = "String"
  val Int: String             = "Int"
  val Float: String           = "Float"
  val Double: String          = "Double"
  val Bool: String            = "Bool"
  val Nil: String             = "Nil"
  val GlobalNamespace: String = NamespaceTraversal.globalNamespaceName

  val SwiftTypes: List[String] = List(Any, Nil, Character, String, Int, Float, Double, Bool)

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
    "&+"   -> Operators.plus,
    "+"    -> Operators.plus,
    "&+="  -> Operators.assignmentPlus,
    "&-"   -> Operators.minus,
    "-"    -> Operators.minus,
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
