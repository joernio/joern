package io.joern.javasrc2cpg.util

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.ast.expr.{
  BooleanLiteralExpr,
  CharLiteralExpr,
  DoubleLiteralExpr,
  Expression,
  IntegerLiteralExpr,
  LiteralExpr,
  LongLiteralExpr,
  NullLiteralExpr,
  StringLiteralExpr,
  TextBlockLiteralExpr
}
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.resolution.declarations.{ResolvedMethodLikeDeclaration, ResolvedTypeDeclaration}
import com.github.javaparser.resolution.types.{
  ResolvedArrayType,
  ResolvedReferenceType,
  ResolvedType,
  ResolvedTypeVariable
}
import io.joern.javasrc2cpg.util.TypeInfoProvider.{TypeConstants, UnresolvedTypeDefault}
import org.slf4j.LoggerFactory

import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object JP2JavaSrcTypeAdapter {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def simpleResolvedTypeFullName(resolvedType: ResolvedType): Option[String] = {
    resolvedType match {
      case resolvedTypeVariable: ResolvedTypeVariable =>
        val extendsBoundOption = resolvedTypeVariable.asTypeParameter().getBounds.asScala.find(_.isExtends)
        extendsBoundOption match {
          case Some(extendsBound) =>
            resolvedTypeFullName(extendsBound.getType)
          case None =>
            Some(TypeConstants.Object)
        }
      case _ =>
        Some(resolvedType.describe())
    }
  }

  def extractNullableName(tryName: Try[String]): String = {
    tryName match {
      case Success(null) => ""

      case Success(name) => name

      case _ => ""
    }
  }

  def resolvedTypeDeclFullName(declaration: ResolvedTypeDeclaration): String = {
    val packageName = extractNullableName(Try(declaration.getPackageName))
    val className   = extractNullableName(Try(declaration.getClassName))
    buildTypeString(packageName, className)
  }

  def resolvedReferenceTypeFullName(resolvedType: ResolvedReferenceType): Option[String] = {
    resolvedType.getTypeDeclaration.toScala match {
      case Some(typeDeclaration) => Some(resolvedTypeDeclFullName(typeDeclaration))

      case None =>
        simpleResolvedTypeFullName(resolvedType)
    }
  }

  def resolvedTypeFullName(resolvedType: ResolvedType): Option[String] = {
    resolvedType match {
      case resolvedReferenceType: ResolvedReferenceType => resolvedReferenceTypeFullName(resolvedReferenceType)

      case _ => simpleResolvedTypeFullName(resolvedType)
    }
  }

  def resolvedMethodLikeDeclFullName(declaration: ResolvedMethodLikeDeclaration): String = {
    val packageName = Try(declaration.getPackageName).getOrElse("")
    val className   = Try(declaration.getClassName).getOrElse(declaration.getName)

    JP2JavaSrcTypeAdapter.buildTypeString(packageName, className)
  }

  def buildTypeString(packageName: String, className: String): String = {
    val dollaredClass = className.replaceAll("\\.", "\\$")
    if (packageName.nonEmpty) {
      s"$packageName.$dollaredClass"
    } else {
      dollaredClass
    }
  }

  def typeForExpression(expr: Expression): Option[String] = {
    Try(expr.calculateResolvedType()) match {
      case Success(resolvedType) => resolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve type for expression $expr")
        None
    }
  }

  def typeForMethodLike(stmt: Resolvable[_ <: ResolvedMethodLikeDeclaration]): Option[String] = {
    Try(stmt.resolve()) match {
      case Success(declaration) =>
        Some(resolvedMethodLikeDeclFullName(declaration))

      case Failure(_) =>
        logger.debug(s"Failed to resolved type for resolvable $stmt")
        None
    }
  }

  def typeForLiteral(literalExpr: LiteralExpr): Option[String] = {
    literalExpr match {
      case _: BooleanLiteralExpr   => Some("boolean")
      case _: CharLiteralExpr      => Some("char")
      case _: DoubleLiteralExpr    => Some("double")
      case _: IntegerLiteralExpr   => Some("int")
      case _: LongLiteralExpr      => Some("long")
      case _: NullLiteralExpr      => Some("null")
      case _: StringLiteralExpr    => Some("java.lang.String")
      case _: TextBlockLiteralExpr => Some("java.lang.String")
      case _                       => None
    }
  }

  def typeNameForTypeDecl(typeDecl: TypeDeclaration[_], fullName: Boolean): String = {
    val javaParserName = if (fullName) {
      typeDecl.getFullyQualifiedName.toScala.getOrElse(typeDecl.getNameAsString)
    } else {
      typeDecl.getNameAsString
    }

    if (typeDecl.isNestedType) {

      typeDecl.getParentNode.toScala match {
        case Some(parentDecl: TypeDeclaration[_]) =>
          typeNameForTypeDecl(parentDecl, fullName) ++ "$" ++ typeDecl.getNameAsString

        case _ =>
          logger.warn("typeNameForTypeDecl expected nested typeDecl to have typeDecl parent.")
          javaParserName
      }

    } else {
      javaParserName
    }
  }
}
