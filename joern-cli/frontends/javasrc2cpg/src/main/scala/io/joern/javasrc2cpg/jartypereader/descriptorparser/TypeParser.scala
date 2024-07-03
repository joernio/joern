package io.joern.javasrc2cpg.jartypereader.descriptorparser

import io.joern.javasrc2cpg.jartypereader.model.Bound.{BoundAbove, BoundBelow}
import io.joern.javasrc2cpg.jartypereader.model.*
import org.slf4j.LoggerFactory

trait TypeParser extends TokenParser {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def unboundWildcard: Parser[UnboundWildcard.type] = "*" ^^ (_ => UnboundWildcard)

  def typeVarSignature: Parser[TypeVariableSignature] = {
    (typeVarStart ~ identifier ~ semicolon) ^^ { case _ ~ name ~ _ => TypeVariableSignature(name) }
  }

  def packageSpecifier: Parser[String] = {
    // TODO Expect these to be short, but potential room for optimizing string concatenation if performance is an issue.
    (identifier ~ slash ~ rep(packageSpecifier)) ^^ {
      case firstId ~ _ ~ Nil             => firstId
      case firstId ~ _ ~ otherSpecifiers => firstId + "." + otherSpecifiers.mkString
    }
  }

  def typeArguments: Parser[List[TypeArgument]] = {
    (openAngle ~ rep1(typeArgument) ~ closeAngle) ^^ { case _ ~ typeArgs ~ _ => typeArgs }
  }

  def typeArgument: Parser[TypeArgument] = {
    val maybeBoundTypeArgument = (opt(wildcardIndicator) ~ referenceTypeSignature) ^^ {
      case Some("-") ~ typeSignature => BoundWildcard(BoundBelow, typeSignature)

      case Some("+") ~ typeSignature => BoundWildcard(BoundAbove, typeSignature)

      case Some(symbol) ~ typeSignature =>
        logger.error(s"Invalid wildcard indicator `$symbol`. Treating as unbound wildcard")
        UnboundWildcard

      case None ~ typeSignature => SimpleTypeArgument(typeSignature)
    }

    maybeBoundTypeArgument | unboundWildcard
  }

  def simpleClassTypeSignature: Parser[NameWithTypeArgs] = {
    (identifier ~ opt(typeArguments)) ^^ { case name ~ maybeTypes =>
      NameWithTypeArgs(name, maybeTypes.getOrElse(Nil))
    }
  }

  def classTypeSignatureSuffix: Parser[NameWithTypeArgs] = {
    (dot ~ simpleClassTypeSignature) ^^ { case _ ~ sig => sig }
  }

  def classTypeSignature: Parser[ClassTypeSignature] = {
    (classTypeStart ~ opt(packageSpecifier) ~ simpleClassTypeSignature ~ rep(classTypeSignatureSuffix) ~ semicolon) ^^ {
      case _ ~ packageSpecifier ~ signature ~ suffix ~ _ =>
        ClassTypeSignature(packageSpecifier, signature, suffix)
    }
  }

  def arrayTypeSignature: Parser[ArrayTypeSignature] = {
    (arrayStart ~ javaTypeSignature) ^^ { case _ ~ sig => ArrayTypeSignature(sig) }
  }

  def javaTypeSignature: Parser[JavaTypeSignature] = referenceTypeSignature | baseType

  def returnType: Parser[JavaTypeSignature] = javaTypeSignature | voidDescriptor

  def referenceTypeSignature: Parser[ReferenceTypeSignature] = {
    classTypeSignature | typeVarSignature | arrayTypeSignature
  }

  def classBound: Parser[Option[ReferenceTypeSignature]] = {
    (colon ~ opt(referenceTypeSignature)) ^^ { case _ ~ maybeSignature => maybeSignature }
  }

  def interfaceBound: Parser[ReferenceTypeSignature] = {
    (colon ~ referenceTypeSignature) ^^ { case _ ~ signature => signature }
  }

  def typeParameter: Parser[TypeParameter] = {
    (identifier ~ classBound ~ rep(interfaceBound)) ^^ { case name ~ classBound ~ interfaceBounds =>
      TypeParameter(name, classBound, interfaceBounds)
    }
  }

  def typeParameters: Parser[List[TypeParameter]] = {
    (openAngle ~ rep1(typeParameter) ~ closeAngle) ^^ { case _ ~ params ~ _ => params }
  }

  def classSignature: Parser[ClassSignature] = {
    (opt(typeParameters) ~ classTypeSignature ~ rep(classTypeSignature)) ^^ {
      case typeParams ~ supClass ~ supInterfaces =>
        ClassSignature(typeParams.getOrElse(Nil), Some(supClass), supInterfaces)
    }
  }

  def methodSignature: Parser[MethodSignature] = {
    (opt(typeParameters) ~ openParen ~ rep(javaTypeSignature) ~ closeParen ~ returnType ~ rep(javaTypeSignature)) ^^ {
      case typeParams ~ _ ~ paramTypes ~ _ ~ returnSignature ~ throwsSignatures =>
        MethodSignature(typeParams.getOrElse(Nil), paramTypes, returnSignature, throwsSignatures)
    }
  }

  def fieldSignature: Parser[ReferenceTypeSignature] = referenceTypeSignature
}
