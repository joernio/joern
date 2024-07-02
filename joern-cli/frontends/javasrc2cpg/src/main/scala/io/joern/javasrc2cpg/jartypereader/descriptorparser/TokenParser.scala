package io.joern.javasrc2cpg.jartypereader.descriptorparser

import io.joern.javasrc2cpg.jartypereader.model.PrimitiveType
import io.joern.javasrc2cpg.jartypereader.model.Model.TypeConstants.*
import org.slf4j.LoggerFactory

import scala.util.parsing.combinator.RegexParsers

trait TokenParser extends RegexParsers {
  private val logger = LoggerFactory.getLogger(classOf[TokenParser])

  case object Colon
  case object Semicolon
  case object Slash
  case object ClassTypeStart
  case object TypeVarStart
  case object ArrayStart
  case object Dot
  case object OpenParen
  case object CloseParen
  case object OpenAngle
  case object CloseAngle
  case object Caret

  private def translateBaseType(descriptor: String): String = {
    descriptor match {
      case "B" => Byte
      case "C" => Char
      case "D" => Double
      case "F" => Float
      case "I" => Int
      case "J" => Long
      case "S" => Short
      case "Z" => Boolean
      // Void is not a BaseType, but sort of acts like one in method return signatures. Since
      // we expect valid descriptors (and types in general) as input, treating void as a base
      // type simplifies the model without introducing a significant possibility for confusion or error.
      case "V" => Void
      case unk => throw new IllegalArgumentException(s"$unk is not a valid base type descriptor.")
    }
  }

  def baseType: Parser[PrimitiveType]             = "[BCDFIJSZ]".r ^^ { t => PrimitiveType(translateBaseType(t)) }
  def voidDescriptor: Parser[PrimitiveType]       = "V" ^^ { t => PrimitiveType(translateBaseType(t)) }
  def identifier: Parser[String]                  = raw"[^.\[/<>:;]+".r ^^ identity
  def wildcardIndicator: Parser[String]           = "[+-]".r ^^ identity
  def colon: Parser[Colon.type]                   = ":" ^^ (_ => Colon)
  def semicolon: Parser[Semicolon.type]           = ";" ^^ (_ => Semicolon)
  def slash: Parser[Slash.type]                   = "/" ^^ (_ => Slash)
  def classTypeStart: Parser[ClassTypeStart.type] = "L" ^^ (_ => ClassTypeStart)
  def typeVarStart: Parser[TypeVarStart.type]     = "T" ^^ (_ => TypeVarStart)
  def arrayStart: Parser[ArrayStart.type]         = "[" ^^ (_ => ArrayStart)
  def dot: Parser[Dot.type]                       = "." ^^ (_ => Dot)
  def openParen: Parser[OpenParen.type]           = "(" ^^ (_ => OpenParen)
  def closeParen: Parser[CloseParen.type]         = ")" ^^ (_ => CloseParen)
  def openAngle: Parser[OpenAngle.type]           = "<" ^^ (_ => OpenAngle)
  def closeAngle: Parser[CloseAngle.type]         = ">" ^^ (_ => CloseAngle)
  def caret: Parser[Caret.type]                   = "^" ^^ (_ => Caret)
}
