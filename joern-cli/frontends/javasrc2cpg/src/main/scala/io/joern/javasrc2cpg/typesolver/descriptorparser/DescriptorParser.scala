package io.joern.javasrc2cpg.typesolver.descriptorparser

import io.joern.javasrc2cpg.typesolver.model.{ClassSignature, MethodSignature}
import org.slf4j.LoggerFactory

import scala.util.Try
import scala.util.parsing.combinator.{Parsers, RegexParsers}

object DescriptorParser extends TypeParser {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val classSignatureDescriptors = List(
      "<T:Ljava/lang/Object;>La/Test$Inner<TT;>;",
      "<T:Ljava/lang/Object;>Ljava/lang/Object;",
      "<S:Ljava/lang/Object;>Ljava/lang/Object;",
      "<S:La/Example;:Ljava/util/List;:Ljava/lang/Comparable;T::Ljava/util/List;:Ljava/lang/Comparable;>Ljava/lang/Object;"
    )

    val methodDescriptor = "<U:La/Example;:Ljava/util/List;>(Ljava/util/List<-Ljava/util/List;>;TU;)V"

    classSignatureDescriptors.foreach { signature =>
      println(parse(classSignature, signature).get)
    }

    println(parse(methodSignature, methodDescriptor).get)
  }

  def parseMethodSignature(descriptor: String): MethodSignature = {
    parse(methodSignature, descriptor) match {
      case Success(signature, _) => signature

      case Failure(err, _) =>
        logger.error(s"parseMethodSignature failed with $err")
        throw new IllegalArgumentException(s"FAILURE: Parsing invalid method signature descriptor $descriptor")

      case Error(err, _) =>
        logger.error(s"parseMethodSignature raised error $err")
        throw new IllegalArgumentException(s"ERROR: Parsing invalid method signature descriptor $descriptor")
    }
  }

  // TODO Re-use code
  def parseClassSignature(descriptor: String): ClassSignature = {
    parse(classSignature, descriptor) match {
      case Success(signature, _) => signature

      case Failure(err, _) =>
        logger.error(s"parseClassSignature failed with $err")
        throw new IllegalArgumentException(s"FAILURE: Parsing invalid class signature descriptor $descriptor")

      case Error(err, _) =>
        logger.error(s"parseClassSignature raised error $err")
        throw new IllegalArgumentException(s"ERROR: Parsing invalid class signature descriptor $descriptor")
    }
  }
}
