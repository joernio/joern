package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.version.Version
import scala.annotation.tailrec

// Precedence in descending order:
// Not
// And
// Or
// And and Or are left assoc
sealed trait VersionConstraint
case class Eq(str: Version) extends VersionConstraint {
  override def toString: String = str.toString
}
case class Any() extends VersionConstraint {
  override def toString: String = "*"
}
case class Gt(constraint: Eq) extends VersionConstraint {
  override def toString: String = s">$constraint"
}
case class Gte(constraint: Eq) extends VersionConstraint {
  override def toString: String = s">=$constraint"
}
case class Lt(constraint: Eq) extends VersionConstraint {
  override def toString: String = s"<$constraint"
}
case class Lte(constraint: Eq) extends VersionConstraint {
  override def toString: String = s"<=$constraint"
}
case class Not(constraint: VersionConstraint) extends VersionConstraint {
  override def toString: String = s"!$constraint"
}
case class And(left: VersionConstraint, right: VersionConstraint) extends VersionConstraint {
  override def toString: String = s"$left && $right"
}
case class Or(left: VersionConstraint, right: VersionConstraint) extends VersionConstraint {
  override def toString: String = s"$left || $right"
}

object VersionConstraint {
  def parse(ctr: String, versionParser: String => Version): VersionConstraint =
    if (isAny(ctr))
    then Any()
    else parseLoop(ctr, versionParser)

  private enum ConstraintSymbol {
    case GT, GTE, LT, LTE, NOT, AND, OR, LPAREN, RPAREN
  }

  /** Shunting yard algorithm */
  @tailrec
  private def parseLoop(
    ctr: String,
    versionParser: String => Version,
    operatorStack: List[ConstraintSymbol] = List(),
    versionStack: List[VersionConstraint] = List()
  ): VersionConstraint = {
    if (ctr.isEmpty) {
      if (operatorStack.isEmpty) {
        versionStack.head
      } else {
        parseLoop(ctr, versionParser, operatorStack.tail, reduceOne(operatorStack.head, versionStack))
      }
    } else if (isWhitespace(ctr)) {
      parseLoop(ctr.dropWhile(isWhitespace), versionParser, operatorStack, versionStack)
    } else if (isLParen(ctr)) {
      parseLoop(ctr.drop("(".length), versionParser, ConstraintSymbol.LPAREN :: operatorStack, versionStack)
    } else if (isRParen(ctr)) {
      val (os, vs) = reduceParenthesizedExpr(operatorStack, versionStack)
      parseLoop(ctr.drop(")".length), versionParser, os, vs)
    } else if (isNot(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.NOT, operatorStack, versionStack)
      parseLoop(ctr.drop("!".length), versionParser, os, vs)
    } else if (isLte(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.LTE, operatorStack, versionStack)
      parseLoop(ctr.drop("<=".length), versionParser, os, vs)
    } else if (isGte(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.GTE, operatorStack, versionStack)
      parseLoop(ctr.drop(">=".length), versionParser, os, vs)
    } else if (isLt(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.LT, operatorStack, versionStack)
      parseLoop(ctr.drop("<".length), versionParser, os, vs)
    } else if (isGt(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.GT, operatorStack, versionStack)
      parseLoop(ctr.drop(">".length), versionParser, os, vs)
    } else if (isAnd(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.AND, operatorStack, versionStack)
      parseLoop(ctr.drop("&&".length), versionParser, os, vs)
    } else if (isOr(ctr)) {
      val (os, vs) = pushOrReduce(ConstraintSymbol.OR, operatorStack, versionStack)
      parseLoop(ctr.drop("||".length), versionParser, os, vs)
    } else if (isIdent(ctr)) {
      val id = ctr.takeWhile(isIdent) // todo-punctuators
      parseLoop(ctr.drop(id.length), versionParser, operatorStack, Eq(versionParser(id)) :: versionStack)
    } else {
      throw new RuntimeException(s"unrecognized dependency constraint expression of string starting at $ctr")
    }
  }

  private def reduceOne(op: ConstraintSymbol, versions: List[VersionConstraint]): List[VersionConstraint] =
    op match
      case ConstraintSymbol.GT  => Gt(versions.head.asInstanceOf[Eq]) :: versions.tail
      case ConstraintSymbol.GTE => Gte(versions.head.asInstanceOf[Eq]) :: versions.tail
      case ConstraintSymbol.LT  => Lt(versions.head.asInstanceOf[Eq]) :: versions.tail
      case ConstraintSymbol.LTE => Lte(versions.head.asInstanceOf[Eq]) :: versions.tail
      case ConstraintSymbol.NOT => Not(versions.head.asInstanceOf[Eq]) :: versions.tail
      case ConstraintSymbol.AND => And(versions(1), versions(0)) :: versions.tail.tail
      case ConstraintSymbol.OR  => Or(versions(1), versions(0)) :: versions.tail.tail
      case _                    => throw new RuntimeException(s"Unexpected op $op")

  private def pushOrReduce(
    op: ConstraintSymbol,
    operatorStack: List[ConstraintSymbol],
    versionStack: List[VersionConstraint]
  ): (List[ConstraintSymbol], List[VersionConstraint]) =
    (op, operatorStack.headOption) match
      case (_, Some(ConstraintSymbol.LPAREN)) =>
        (op :: operatorStack, versionStack)
      case (_, Some(other)) if other != ConstraintSymbol.AND && other != ConstraintSymbol.OR =>
        pushOrReduce(op, operatorStack.tail, reduceOne(other, versionStack))
      case (other, _) if other != ConstraintSymbol.AND && other != ConstraintSymbol.OR =>
        (other :: operatorStack, versionStack)
      case (_, Some(ConstraintSymbol.AND)) =>
        pushOrReduce(op, operatorStack.tail, reduceOne(ConstraintSymbol.AND, versionStack))
      case (ConstraintSymbol.AND, _) =>
        (ConstraintSymbol.AND :: operatorStack, versionStack)
      case (_, Some(ConstraintSymbol.OR)) =>
        pushOrReduce(op, operatorStack.tail, reduceOne(ConstraintSymbol.OR, versionStack))
      case (ConstraintSymbol.OR, _) =>
        (ConstraintSymbol.OR :: operatorStack, versionStack)

  @tailrec
  private def reduceParenthesizedExpr(
    operatorStack: List[ConstraintSymbol],
    versionStack: List[VersionConstraint]
  ): (List[ConstraintSymbol], List[VersionConstraint]) =
    operatorStack.headOption match
      case Some(ConstraintSymbol.LPAREN) => (operatorStack.tail, versionStack)
      case Some(other) =>
        reduceParenthesizedExpr(operatorStack.tail, reduceOne(other, versionStack))
      case None => throw new RuntimeException("Unbalanced parens in dependency constraints")

  private def isLt(ctr: String): Boolean  = ctr.startsWith("<")
  private def isLte(ctr: String): Boolean = ctr.startsWith("<=")
  private def isGt(ctr: String): Boolean  = ctr.startsWith(">")
  private def isGte(ctr: String): Boolean = ctr.startsWith(">=")
  // https://www.compart.com/en/unicode/category no open or close punctuators like open or close paren
  private def isIdent(ctr: String): Boolean      = ctr.matches("^[\\p{Alpha}\\p{Digit}\\p{Pd}\\p{Pc}\\p{Po}].*?")
  private def isIdent(ctr: Char): Boolean        = isIdent("" + ctr)
  private def isWhitespace(ctr: String): Boolean = ctr.matches("^\\p{Space}.*?")
  private def isWhitespace(ctr: Char): Boolean   = isWhitespace("" + ctr)
  private def isEq(ctr: String): Boolean         = ctr.startsWith("=") || ctr.startsWith("==") || isIdent(ctr)
  private def isNot(ctr: String): Boolean        = ctr.startsWith("!")
  private def isAnd(ctr: String): Boolean        = ctr.startsWith("&&")
  private def isOr(ctr: String): Boolean         = ctr.startsWith("||")
  private def isAny(ctr: String): Boolean        = ctr.startsWith("*")
  private def isLParen(ctr: String): Boolean     = ctr.startsWith("(")
  private def isRParen(ctr: String): Boolean     = ctr.startsWith(")")
}
