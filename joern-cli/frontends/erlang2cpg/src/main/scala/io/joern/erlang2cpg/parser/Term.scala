package io.joern.erlang2cpg.parser

import java.nio.file.Path
import java.util.Base64

/** Erlang terms as returned by epp. Tuple subscripts follow the OTP abstract format (zero based here). */
sealed trait Term {
  import Term.*

  def items: Vector[Term] = this match {
    case Tuple(values)    => values
    case ListTerm(values) => values
    case _                => Vector.empty
  }

  def apply(index: Int): Term = items(index)

  def tag: String = this match {
    case Tuple(Atom(kind) +: _) => kind
    case _                      => ""
  }

  def text: String = this match {
    case Atom(value)      => value
    case Integer(value)   => value.toString
    case FloatTerm(value) => value.toString
    case ListTerm(values) if values.forall {
          case Integer(value) => value.isValidInt && Character.isValidCodePoint(value.toInt)
          case _              => false
        } =>
      val chars = values.collect { case Integer(value) => value.toInt }.toArray
      new String(chars, 0, chars.length)
    case Tuple(values)    => values.map(_.text).mkString("{", ",", "}")
    case ListTerm(values) => values.map(_.text).mkString("[", ",", "]")
    case other            => other.toString
  }

  private def location: Option[Term] = items.lift(1).flatMap {
    case ListTerm(properties) =>
      properties.collectFirst { case Tuple(Vector(Atom("location"), position)) => position }
    case position => Some(position)
  }

  def line: Option[Int] = location.collect {
    case Integer(value) if value.isValidInt                   => value.toInt
    case Tuple(Vector(Integer(value), _)) if value.isValidInt => value.toInt
  }

  def column: Option[Int] = location.collect {
    case Tuple(Vector(_, Integer(value))) if value.isValidInt => value.toInt
  }
}

object Term {
  final case class Atom(value: String)                        extends Term
  final case class Integer(value: BigInt)                     extends Term
  final case class FloatTerm(value: Double)                   extends Term
  final case class Tuple(values: Vector[Term])                extends Term
  final case class ListTerm(values: Vector[Term])             extends Term
  final case class Improper(values: Vector[Term], tail: Term) extends Term
  final case class MapTerm(entries: Vector[(Term, Term)])     extends Term
  final case class Binary(bytes: Vector[Byte])                extends Term
  final case class Bits(bytes: Vector[Byte], length: Int)     extends Term

  private[parser] def decode(node: ujson.Value): Term = {
    def children          = node(1).arr.iterator.map(decode).toVector
    def bytes(index: Int) = Base64.getDecoder.decode(node(index).str).toVector
    node(0).str match {
      case "atom"     => Atom(node(1).str)
      case "integer"  => Integer(BigInt(node(1).str))
      case "float"    => FloatTerm(node(1).str.toDouble)
      case "tuple"    => Tuple(children)
      case "list"     => ListTerm(children)
      case "improper" => Improper(children, decode(node(2)))
      case "map"      => MapTerm(node(1).arr.iterator.map(pair => decode(pair(0)) -> decode(pair(1))).toVector)
      case "binary"   => Binary(bytes(1))
      case "bits"     => Bits(bytes(2), node(1).num.toInt)
      case kind       => throw new IllegalArgumentException(s"Unknown OTP helper term tag '$kind'")
    }
  }
}

final case class Form(file: String, term: Term)
final case class ParsedModule(file: Path, forms: Vector[Form], autoImports: Set[(String, Int)]) {
  val moduleName: String = forms.collect {
    case Form(_, Term.Tuple(Vector(Term.Atom("attribute"), _, Term.Atom("module"), Term.Atom(name)))) => name
  } match {
    case Vector(name) => name
    case _            => throw new SourceParseException(s"$file: expected one -module declaration")
  }
}
