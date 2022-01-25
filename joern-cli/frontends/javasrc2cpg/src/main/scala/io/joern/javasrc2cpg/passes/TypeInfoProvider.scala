package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{
  ConstructorDeclaration,
  EnumConstantDeclaration,
  MethodDeclaration,
  TypeDeclaration,
  VariableDeclarator
}
import com.github.javaparser.ast.expr.{
  BooleanLiteralExpr,
  CharLiteralExpr,
  DoubleLiteralExpr,
  Expression,
  IntegerLiteralExpr,
  LiteralExpr,
  LongLiteralExpr,
  MethodCallExpr,
  NameExpr,
  NullLiteralExpr,
  StringLiteralExpr,
  TextBlockLiteralExpr,
  ThisExpr
}
import com.github.javaparser.ast.nodeTypes.NodeWithType
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.resolution.declarations.{
  ResolvedDeclaration,
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedReferenceTypeDeclaration,
  ResolvedTypeDeclaration,
  ResolvedTypeParameterDeclaration
}
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

class TypeInfoProvider(global: Global) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Add `typeName` to a global map and return it. The
    * map is later passed to a pass that creates TYPE
    * nodes for each key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.putIfAbsent(typeName, true)
    typeName
  }

  private def simpleResolvedTypeFullName(resolvedType: ResolvedType, typeParameterString: String = ""): String = {
    resolvedType.describe()
  }

  private def buildTypeString(packageName: String, className: String, typeParameterString: String): String = {
    val dollaredClass = className.replaceAll("\\.", "\\$")
    if (packageName.nonEmpty) {
      s"$packageName.$dollaredClass$typeParameterString"
    } else {
      s"$dollaredClass$typeParameterString"
    }
  }

  private def resolvedTypeDeclFullName(
      declaration: ResolvedTypeDeclaration,
      typeParameterString: String = ""
  ): String = {
    buildTypeString(declaration.getPackageName, declaration.getClassName, typeParameterString)
  }

  private def resolvedTypeParamFullName(
      declaration: ResolvedTypeParameterDeclaration,
      typeParameterString: String = ""
  ): String = {
    buildTypeString(declaration.getPackageName, declaration.getClassName, typeParameterString)
  }

  private def resolvedMethodLikeDeclFullName(
      declaration: ResolvedMethodLikeDeclaration,
      typeParameterString: String = ""
  ): String = {
    val baseString = buildTypeString(declaration.getPackageName, declaration.getClassName, typeParameterString)
    val typeParameters =
      declaration.getTypeParameters.asScala.map(resolvedTypeParamFullName(_, typeParameterString)).toList

    val typeParamString = if (typeParameters.nonEmpty) {
      s"<${typeParameters.mkString(",")}>"
    } else {
      ""
    }

    s"$baseString$typeParamString"
  }

  private def buildTypeParameterString(typeParams: Iterable[ResolvedType]): String = {
    typeParams match {
      case Nil => ""

      case _ =>
        val innerString = typeParams
          .map { param =>
            simpleResolvedTypeFullName(param)
          }
          .mkString(",")
        s"<$innerString>"
    }
  }

  private def resolvedReferenceTypeFullName(resolvedType: ResolvedReferenceType): String = {
    val typeParamString = buildTypeParameterString(resolvedType.typeParametersValues().asScala)

    resolvedType.getTypeDeclaration.toScala match {
      case Some(typeDeclaration) => resolvedTypeDeclFullName(typeDeclaration, typeParamString)

      case None =>
        simpleResolvedTypeFullName(resolvedType, typeParamString)
    }
  }

  private def resolvedTypeFullName(resolvedType: ResolvedType): String = {
    resolvedType match {
      case resolvedReferenceType: ResolvedReferenceType => resolvedReferenceTypeFullName(resolvedReferenceType)

      case _ => simpleResolvedTypeFullName(resolvedType)
    }
  }

  private def typeFullNameForTypeDecl(typeDecl: TypeDeclaration[_]): String = {
    val javaParserName = typeDecl.getFullyQualifiedName.toScala.getOrElse(typeDecl.getNameAsString)

    if (typeDecl.isNestedType) {

      typeDecl.getParentNode.toScala match {
        case Some(parentDecl: TypeDeclaration[_]) =>
          typeFullNameForTypeDecl(parentDecl) ++ "$" ++ typeDecl.getNameAsString

        case _ =>
          logger.warn("typeFullNameForTypeDecl expected nested typeDecl to have typeDecl parent.")
          javaParserName
      }

    } else {
      javaParserName
    }
  }

  def getTypeFullName(typeDecl: TypeDeclaration[_]): String = {
    registerType(typeFullNameForTypeDecl(typeDecl))
  }

  def getTypeFullName(node: NodeWithType[_, _ <: Resolvable[ResolvedType]]): String = {
    val typeFullName = Try(node.getType.resolve()) match {
      case Success(resolvedType: ResolvedReferenceType) => resolvedReferenceTypeFullName(resolvedType)

      case Success(resolvedType: ResolvedType) => simpleResolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.info(s"Resolving type ${node.getTypeAsString} failed. Falling back to unresolved default.")
        "<unresolved>." ++ node.getTypeAsString
    }

    registerType(typeFullName)
  }

  def getTypeFullName(typ: ClassOrInterfaceType): String = {
    val typeFullName = Try(typ.resolve) match {
      case Success(resolvedType) => resolvedReferenceTypeFullName(resolvedType)

      case Failure(_) =>
        logger.info(s"Failed to resolve class type ${typ.getNameAsString}. Falling back to unresolved default.")
        "<unresolved>." ++ typ.getNameAsString
    }

    registerType(typeFullName)
  }

  def getTypeFullName(enumConstant: EnumConstantDeclaration): String = {
    val typeFullName = Try(enumConstant.resolve()) match {
      case Success(resolvedDeclaration) =>
        resolvedTypeFullName(resolvedDeclaration.getType)

      case Failure(_) =>
        logger.info(s"Failed to resolve enum entry type for ${enumConstant.getNameAsString}")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getReturnType(node: Resolvable[ResolvedMethodDeclaration]): String = {
    val typeFullName = Try(node.resolve().getReturnType) match {
      case Success(resolved) => resolvedTypeFullName(resolved)

      case Failure(_) =>
        logger.info(s"Failed to resolve return type. Defaulting to <empty>.")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getTypeFullName(nameExpr: NameExpr): String = {
    val typeFullName = Try(nameExpr.resolve()) match {
      case Success(resolvedValueDeclaration) =>
        resolvedTypeFullName(resolvedValueDeclaration.getType)

      case Failure(_) =>
        logger.info(s"Failed to resolved type for nameExpr ${nameExpr.getNameAsString}. Falling back to name.")
        nameExpr.getNameAsString

    }

    registerType(typeFullName)
  }

  def getTypeFullName(thisExpr: ThisExpr): String = {
    val typeFullName = Try(thisExpr.resolve()) match {
      case Success(declaration) => resolvedTypeDeclFullName(declaration)

      case Failure(_) =>
        logger.info(s"Failed to resolve type for `this` expr. Defaulting to <empty>")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getMethodLikeTypeFullName(methodLike: Resolvable[_ <: ResolvedMethodLikeDeclaration]): String = {
    val typeFullName = Try(methodLike.resolve()) match {
      case Success(declaration) => resolvedMethodLikeDeclFullName(declaration)

      case Failure(_) =>
        logger.info(s"Failed to resolve type for method-like ${methodLike}. Defaulting to <empty>")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getLiteralTypeFullName(literalExpr: LiteralExpr): String = {
    val typeFullName = literalExpr match {
      case _: BooleanLiteralExpr   => "boolean"
      case _: CharLiteralExpr      => "char"
      case _: DoubleLiteralExpr    => "double"
      case _: IntegerLiteralExpr   => "int"
      case _: LongLiteralExpr      => "long"
      case _: NullLiteralExpr      => "null"
      case _: StringLiteralExpr    => "java.lang.String"
      case _: TextBlockLiteralExpr => "java.lang.String"
      case _                       => "<empty>"
    }

    logger.info(s"Processing type for literal ${literalExpr.getClass}: $typeFullName")
    registerType(typeFullName)
  }

  def getTypeFullName(invocation: ExplicitConstructorInvocationStmt): String = {
    val typeFullName = Try(invocation.resolve()) match {
      case Success(declaration) =>
        resolvedMethodLikeDeclFullName(declaration)

      case Failure(_) =>
        logger.info(s"Could not resolve type for constructor invocation $invocation. Defaulting to <empty>.")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getTypeForExpression(expr: Expression): String = {
    val typeFullName = Try(expr.calculateResolvedType()) match {
      case Success(resolvedType) => resolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.info(s"Could not resolve type for expr $expr")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getInitializerType(variableDeclarator: VariableDeclarator): Option[String] = {
    variableDeclarator.getInitializer.toScala flatMap { initializer =>
      Try(initializer.calculateResolvedType()) match {
        case Success(resolvedType) =>
          Some(
            registerType(resolvedTypeFullName(resolvedType))
          )

        case Failure(_) =>
          logger.info(s"Failed to resolve type for initializer ${initializer.toString}")
          None
      }
    }
  }
}

object TypeInfoProvider {
  def apply(global: Global): TypeInfoProvider = {
    new TypeInfoProvider(global)
  }
}
