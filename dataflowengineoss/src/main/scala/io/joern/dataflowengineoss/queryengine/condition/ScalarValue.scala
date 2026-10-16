package io.joern.dataflowengineoss.queryengine.condition

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Call,
  Expression,
  Identifier,
  Literal,
  Local,
  Member,
  MethodParameterIn,
  MethodRef,
  TypeRef
}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.allAssignmentTypes

import scala.util.Try

final case class ScalarValue(knownZero: BigInt, knownOne: BigInt, width: Int = 64) {
  require(width > 0)

  val mask: BigInt = (BigInt(1) << width) - 1

  require(knownZero >= 0 && knownOne >= 0)
  require(((knownZero | knownOne) & ~mask) == 0)
  require((knownZero & knownOne) == 0)

  def constant: Option[BigInt] = Option.when((knownZero | knownOne) == mask)(knownOne)

  def signedConstant: Option[BigInt] = constant.map { value =>
    if (value.testBit(width - 1)) value - (BigInt(1) << width) else value
  }

  def truth: Option[Boolean] = {
    if (knownOne != 0) Some(true)
    else Option.when(knownZero == mask)(false)
  }

  def join(other: ScalarValue): ScalarValue = {
    if (this == other) this
    else if (width == other.width) ScalarValue(knownZero & other.knownZero, knownOne & other.knownOne, width)
    else ScalarValue(0, 0, math.max(width, other.width))
  }

  def containsBits(bits: BigInt): Option[Boolean] = {
    require(bits >= 0)
    if ((bits & ~mask) != 0 || (knownZero & bits) != 0) Some(false)
    else Option.when((knownOne & bits) == bits)(true)
  }

  def resize(targetWidth: Int, signed: Boolean): ScalarValue = {
    require(targetWidth > 0)
    val targetMask = (BigInt(1) << targetWidth) - 1
    if (targetWidth <= width) ScalarValue(knownZero & targetMask, knownOne & targetMask, targetWidth)
    else {
      val high = targetMask ^ mask
      val sign = BigInt(1) << (width - 1)
      if (!signed || (knownZero & sign) != 0) ScalarValue(knownZero | high, knownOne, targetWidth)
      else if ((knownOne & sign) != 0) ScalarValue(knownZero, knownOne | high, targetWidth)
      else ScalarValue(knownZero, knownOne, targetWidth)
    }
  }
}

object ScalarValue {
  val Unknown: ScalarValue = ScalarValue(0, 0)

  def exact(value: BigInt, width: Int = 64): ScalarValue = {
    val mask       = (BigInt(1) << width) - 1
    val normalized = value & mask
    ScalarValue(mask ^ normalized, normalized, width)
  }

  def boolean(value: Boolean): ScalarValue = exact(if (value) BigInt(1) else BigInt(0))
}

final class ScalarEvaluation(integerTypes: Map[String, (Int, Boolean)] = Map.empty) {
  import ScalarValue.*

  private case class IntegerType(width: Int, signed: Boolean)

  private val fixedTypes = (for {
    width            <- List(8, 16, 32, 64)
    (prefix, signed) <- List("int" -> true, "uint" -> false)
  } yield s"$prefix${width}_t" -> IntegerType(width, signed)).toMap ++
    Map("bool" -> IntegerType(1, false), "_Bool" -> IntegerType(1, false))

  private val types = fixedTypes ++ integerTypes.map { case (name, (width, signed)) =>
    require(width > 0 && width <= 64)
    normalizeType(name) -> IntegerType(width, signed)
  }

  private val integerToken = "([+-]?)(0[xX][0-9a-fA-F]+|0[bB][01]+|0[0-7]*|[1-9][0-9]*)([uUlL]*)".r
  private val suffixes     = Set("", "u", "l", "ll", "ul", "ull", "lu", "llu")
  private val lowBits      = (BigInt(1) << 16) - 1
  private val shifts       = Set(
    Operators.shiftLeft,
    Operators.assignmentShiftLeft,
    Operators.arithmeticShiftRight,
    Operators.logicalShiftRight,
    Operators.assignmentArithmeticShiftRight,
    Operators.assignmentLogicalShiftRight
  )
  private val binaryArithmetic = Set(
    Operators.addition,
    Operators.subtraction,
    Operators.multiplication,
    Operators.division,
    Operators.modulo,
    Operators.and,
    Operators.or,
    Operators.xor
  )
  private val booleanOperators = Set(
    Operators.equals,
    Operators.notEquals,
    Operators.lessThan,
    Operators.lessEqualsThan,
    Operators.greaterThan,
    Operators.greaterEqualsThan,
    Operators.logicalNot,
    Operators.logicalAnd,
    Operators.logicalOr
  )

  def literal(node: Literal): ScalarValue = {
    val value = node.code.trim match {
      case "true"                                                                      => Some(BigInt(1))
      case "false"                                                                     => Some(BigInt(0))
      case integerToken(sign, digits, suffix) if suffixes.contains(suffix.toLowerCase) =>
        val (radix, body) =
          if (digits.startsWith("0x") || digits.startsWith("0X")) (16, digits.drop(2))
          else if (digits.startsWith("0b") || digits.startsWith("0B")) (2, digits.drop(2))
          else if (digits.startsWith("0") && digits.length > 1) (8, digits.drop(1))
          else (10, digits)
        Try(BigInt(body, radix)).toOption.map(number => if (sign == "-") -number else number)
      case _ => None
    }
    value.filter(number => number >= -(BigInt(1) << 63) && number < (BigInt(1) << 64)) match {
      case Some(number) => convertKnown(exact(number), node.typeFullName)
      case None         => Unknown
    }
  }

  def convert(value: ScalarValue, typeFullName: String): ScalarValue = {
    types.get(normalizeType(typeFullName)) match {
      case Some(tpe) => convertInteger(value, tpe)
      case None      => Unknown
    }
  }

  def operator(call: Call, arguments: Vector[ScalarValue]): ScalarValue = {
    val values = arguments.map(value => if (value.width == 64) value else value.resize(64, signed = true))
    def argument(index: Int): ScalarValue = values.lift(index).getOrElse(Unknown)
    val left                              = argument(0)
    val right                             = argument(1)
    val resultType                        = expressionIntegerType(call, Set.empty)

    val result = call.name match {
      case Operators.assignment => right
      case Operators.cast       =>
        (for (value <- values.lastOption; tpe <- resultType) yield convertInteger(value, tpe)).getOrElse(Unknown)
      case Operators.plus                          => left
      case Operators.and | Operators.assignmentAnd =>
        ScalarValue(left.knownZero | right.knownZero, left.knownOne & right.knownOne)
      case Operators.or | Operators.assignmentOr =>
        ScalarValue(left.knownZero & right.knownZero, left.knownOne | right.knownOne)
      case Operators.xor | Operators.assignmentXor =>
        ScalarValue(
          (left.knownZero & right.knownZero) | (left.knownOne & right.knownOne),
          (left.knownOne & right.knownZero) | (left.knownZero & right.knownOne)
        )
      case Operators.not        => ScalarValue(left.knownOne, left.knownZero)
      case Operators.logicalNot => left.truth.map(value => boolean(!value)).getOrElse(Unknown)
      case Operators.logicalAnd =>
        (left.truth, right.truth) match {
          case (Some(false), _) | (_, Some(false)) => boolean(false)
          case (Some(true), Some(true))            => boolean(true)
          case _                                   => Unknown
        }
      case Operators.logicalOr =>
        (left.truth, right.truth) match {
          case (Some(true), _) | (_, Some(true)) => boolean(true)
          case (Some(false), Some(false))        => boolean(false)
          case _                                 => Unknown
        }
      case Operators.equals | Operators.notEquals => equality(call, left, right)
      case Operators.lessThan | Operators.lessEqualsThan | Operators.greaterThan | Operators.greaterEqualsThan =>
        comparison(call, left, right)
      case Operators.conditional =>
        left.truth match {
          case Some(true)  => argument(1)
          case Some(false) => argument(2)
          case None        => argument(1).join(argument(2))
        }
      case _ => arithmetic(call, left, right)
    }

    call.name match {
      case Operators.equals | Operators.notEquals | Operators.lessThan | Operators.lessEqualsThan |
          Operators.greaterThan | Operators.greaterEqualsThan | Operators.logicalNot | Operators.logicalAnd |
          Operators.logicalOr =>
        result
      case Operators.assignment | Operators.assignmentAnd | Operators.assignmentOr | Operators.assignmentXor
          if resultType.isEmpty =>
        result.constant.filter(_ <= 1).map(exact(_)).getOrElse(Unknown)
      case Operators.and | Operators.or | Operators.xor | Operators.not if resultType.isEmpty =>
        result.constant
          .filter(_ <= 32767)
          .map(exact(_))
          .getOrElse(ScalarValue(result.knownZero & lowBits, result.knownOne & lowBits))
      case _ => resultType.map(convertInteger(result, _)).getOrElse(result)
    }
  }

  private def equality(call: Call, left: ScalarValue, right: ScalarValue): ScalarValue = {
    val converted    = comparisonType(call).map(tpe => (convertInteger(left, tpe), convertInteger(right, tpe)))
    val (a, b)       = converted.getOrElse((left, right))
    val comparedMask = if (converted.nonEmpty) a.mask else lowBits
    val conflict     = ((a.knownOne & b.knownZero) | (a.knownZero & b.knownOne)) & comparedMask
    val equal        =
      if (conflict != 0) Some(false)
      else
        for {
          x <- a.constant
          y <- b.constant
          if converted.nonEmpty || (x <= 32767 && y <= 32767)
        } yield x == y
    equal.map(value => boolean(if (call.name == Operators.equals) value else !value)).getOrElse(Unknown)
  }

  private def comparison(call: Call, left: ScalarValue, right: ScalarValue): ScalarValue = {
    val values = comparisonType(call) match {
      case Some(tpe) =>
        for {
          x <- integerConstant(left, tpe)
          y <- integerConstant(right, tpe)
        } yield (x, y)
      case None =>
        for {
          x <- left.constant.filter(_ <= 32767)
          y <- right.constant.filter(_ <= 32767)
        } yield (x, y)
    }
    values
      .map { case (x, y) =>
        boolean(call.name match {
          case Operators.lessThan          => x < y
          case Operators.lessEqualsThan    => x <= y
          case Operators.greaterThan       => x > y
          case Operators.greaterEqualsThan => x >= y
        })
      }
      .getOrElse(Unknown)
  }

  private def arithmetic(call: Call, left: ScalarValue, right: ScalarValue): ScalarValue = {
    val shift  = shifts.contains(call.name)
    val result = for {
      target <- expressionIntegerType(call, Set.empty)
      tpe    <-
        if (!allAssignmentTypes.contains(call.name)) Some(target)
        else if (shift)
          call.argument.l
            .sortBy(_.argumentIndex)
            .headOption
            .flatMap(expressionIntegerType(_, Set.empty))
            .flatMap(promote)
        else comparisonType(call)
      x <- integerConstant(left, tpe)
      y <-
        if (call.name == Operators.minus) Some(BigInt(0))
        else if (shift) right.signedConstant
        else integerConstant(right, tpe)
      value <- call.name match {
        case Operators.addition | Operators.assignmentPlus                 => Some(x + y)
        case Operators.subtraction | Operators.assignmentMinus             => Some(x - y)
        case Operators.multiplication | Operators.assignmentMultiplication => Some(x * y)
        case Operators.division | Operators.assignmentDivision
            if y != 0 && !(tpe.signed && x == -(BigInt(1) << (tpe.width - 1)) && y == -1) =>
          Some(x / y)
        case Operators.modulo | Operators.assignmentModulo
            if y != 0 && !(tpe.signed && x == -(BigInt(1) << (tpe.width - 1)) && y == -1) =>
          Some(x % y)
        case Operators.minus => Some(-x)
        case Operators.shiftLeft | Operators.assignmentShiftLeft
            if y >= 0 && y < tpe.width && (!tpe.signed || x >= 0) =>
          Some(x << y.toInt)
        case Operators.arithmeticShiftRight | Operators.logicalShiftRight | Operators.assignmentArithmeticShiftRight |
            Operators.assignmentLogicalShiftRight if y >= 0 && y < tpe.width && x >= 0 =>
          Some(x >> y.toInt)
        case _ => None
      }
      if !tpe.signed || (value >= -(BigInt(1) << (tpe.width - 1)) && value < (BigInt(1) << (tpe.width - 1)))
    } yield convertInteger(exact(value), tpe)
    result.getOrElse(Unknown)
  }

  private def comparisonType(call: Call): Option[IntegerType] = {
    val operandTypes = call.argument.l
      .sortBy(_.argumentIndex)
      .take(2)
      .map(expressionIntegerType(_, Set.empty).flatMap(promote))
    operandTypes match {
      case List(Some(a), Some(b)) => Some(commonType(a, b))
      case _                      => None
    }
  }

  private def commonType(left: IntegerType, right: IntegerType): IntegerType = {
    if (left.signed == right.signed) IntegerType(math.max(left.width, right.width), left.signed)
    else {
      val unsigned = if (!left.signed) left else right
      val signed   = if (left.signed) left else right
      if (signed.width > unsigned.width) signed else unsigned
    }
  }

  private def expressionIntegerType(node: Expression, seen: Set[Long]): Option[IntegerType] = {
    if (seen(node.id)) None
    else
      node match {
        case call: Call =>
          val visited                                   = seen + node.id
          val direct                                    = types.get(normalizeType(call.typeFullName))
          lazy val arguments                            = call.argument.toVector.sortBy(_.argumentIndex)
          def argument(index: Int): Option[IntegerType] =
            arguments.lift(index).flatMap(expressionIntegerType(_, visited))
          def promoted(index: Int): Option[IntegerType]          = argument(index).flatMap(promote)
          def common(left: Int, right: Int): Option[IntegerType] =
            for (a <- promoted(left); b <- promoted(right)) yield commonType(a, b)

          val inferred =
            if (allAssignmentTypes.contains(call.name)) argument(0)
            else if (call.name == Operators.cast) {
              arguments
                .collectFirst { case ref: TypeRef => ref }
                .flatMap(ref => types.get(normalizeType(ref.typeFullName)))
            } else if (shifts.contains(call.name)) promoted(0)
            else if (binaryArithmetic.contains(call.name)) common(0, 1)
            else if (Set(Operators.plus, Operators.minus, Operators.not).contains(call.name)) promoted(0)
            else if (booleanOperators.contains(call.name)) types.get("int")
            else if (call.name == Operators.conditional) common(1, 2)
            else if (call.dispatchType == DispatchTypes.INLINED) {
              call.macroExpansion
                .collect { case expression: Expression => expression }
                .toVector
                .lastOption
                .flatMap(expressionIntegerType(_, visited))
            } else None
          inferred.orElse(direct)
        case _ => expressionType(node).flatMap(name => types.get(normalizeType(name)))
      }
  }

  private def expressionType(node: Expression): Option[String] = node match {
    case value: Call       => Some(value.typeFullName)
    case value: Identifier =>
      val direct = value.typeFullName
      if (types.contains(normalizeType(direct))) Some(direct)
      else
        value.refsTo.toVector match {
          case Vector(declaration: Local)             => Some(declaration.typeFullName)
          case Vector(declaration: MethodParameterIn) => Some(declaration.typeFullName)
          case Vector(declaration: Member)            => Some(declaration.typeFullName)
          case _                                      => Some(direct)
        }
    case value: Literal   => Some(value.typeFullName)
    case value: TypeRef   => Some(value.typeFullName)
    case value: MethodRef => Some(value.typeFullName)
    case value: Block     => Some(value.typeFullName)
    case _                => None
  }

  private def integerConstant(value: ScalarValue, tpe: IntegerType): Option[BigInt] = {
    val converted = convertInteger(value, tpe)
    if (tpe.signed) converted.signedConstant else converted.constant
  }

  private def promote(tpe: IntegerType): Option[IntegerType] = {
    types.get("int") match {
      case Some(integer) if tpe.width < integer.width => Some(integer)
      case Some(_)                                    => Some(tpe)
      case None                                       => None
    }
  }

  private def convertKnown(value: ScalarValue, typeFullName: String): ScalarValue =
    types.get(normalizeType(typeFullName)).map(convertInteger(value, _)).getOrElse(value)

  private def convertInteger(value: ScalarValue, tpe: IntegerType): ScalarValue = {
    if (tpe.width == 1 && !tpe.signed) value.truth.map(boolean).getOrElse(Unknown)
    else value.resize(tpe.width, signed = tpe.signed).resize(64, signed = tpe.signed)
  }

  private def normalizeType(name: String): String =
    name.replaceAll("\\b(const|volatile|restrict)\\b", "").trim.replaceAll("\\s+", " ")
}
