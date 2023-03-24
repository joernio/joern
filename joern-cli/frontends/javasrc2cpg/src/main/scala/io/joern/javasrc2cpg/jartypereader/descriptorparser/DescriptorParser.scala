package io.joern.javasrc2cpg.jartypereader.descriptorparser

import io.joern.javasrc2cpg.jartypereader.model.{ClassSignature, MethodSignature, ReferenceTypeSignature}
import org.slf4j.LoggerFactory

import scala.util.Try
import scala.util.parsing.combinator.{Parsers, RegexParsers}

object DescriptorParser extends TypeParser {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def parseMethodSignature(descriptor: String): MethodSignature = {
    parseSignature(methodSignature, descriptor)
  }

  def parseClassSignature(descriptor: String): ClassSignature = {
    parseSignature(classSignature, descriptor)
  }

  def parseFieldSignature(descriptor: String): ReferenceTypeSignature = {
    parseSignature(fieldSignature, descriptor)
  }

  private def parseSignature[T](parser: Parser[T], descriptor: String): T = {
    parse(parser, descriptor) match {
      case Success(signature, _) => signature

      case Failure(err, _) =>
        logger.error(s"parseClassSignature failed with $err")
        throw new IllegalArgumentException(s"FAILURE: Parsing invalid signature descriptor $descriptor")

      case Error(err, _) =>
        logger.error(s"parseClassSignature raised error $err")
        throw new IllegalArgumentException(s"ERROR: Parsing invalid signature descriptor $descriptor")
    }
  }
}
