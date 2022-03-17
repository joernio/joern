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
  ResolvedParameterDeclaration,
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

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map. Skip the `ANY` type, since this is created by default.
    */
  def registerType(typeName: String): String = {
    if (typeName != "ANY") {
      global.usedTypes.putIfAbsent(typeName, true)
    }
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
    val packageName = Try(declaration.getPackageName).getOrElse("")
    val className   = Try(declaration.getClassName).getOrElse(declaration.getName)
    buildTypeString(packageName, className, typeParameterString)
  }

  private def resolvedTypeParamFullName(
    declaration: ResolvedTypeParameterDeclaration,
    typeParameterString: String = ""
  ): String = {
    val packageName = Try(declaration.getPackageName).getOrElse("")
    val className   = Try(declaration.getClassName).getOrElse(declaration.getName)
    buildTypeString(packageName, className, typeParameterString)
  }

  private def resolvedMethodLikeDeclFullName(
    declaration: ResolvedMethodLikeDeclaration,
    typeParameterString: String = ""
  ): String = {
    val packageName = Try(declaration.getPackageName).getOrElse("")
    val className   = Try(declaration.getClassName).getOrElse(declaration.getName)
    val baseString  = buildTypeString(packageName, className, typeParameterString)
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

  private def typeNameForTypeDecl(typeDecl: TypeDeclaration[_], fullName: Boolean): String = {
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

  def getTypeName(typeDecl: TypeDeclaration[_], fullName: Boolean = true): String = {
    val typeName = typeNameForTypeDecl(typeDecl, fullName)
    if (fullName) {
      registerType(typeName)
    } else {
      typeName
    }
  }

  def getTypeFullName(node: NodeWithType[_, _ <: Resolvable[ResolvedType]]): String = {
    val typeFullName = Try(node.getType.resolve()) match {
      case Success(resolvedType: ResolvedReferenceType) => resolvedReferenceTypeFullName(resolvedType)

      case Success(resolvedType: ResolvedType) => simpleResolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Resolving type ${node.getTypeAsString} failed. Falling back to unresolved default.")
        "<unresolved>." ++ node.getTypeAsString
    }

    registerType(typeFullName)
  }

  def getTypeFullName(typ: ClassOrInterfaceType): String = {
    val typeFullName = Try(typ.resolve) match {
      case Success(resolvedType) => resolvedReferenceTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve class type ${typ.getNameAsString}. Falling back to unresolved default.")
        "<unresolved>." ++ typ.getNameAsString
    }

    registerType(typeFullName)
  }

  def getTypeFullName(enumConstant: EnumConstantDeclaration): String = {
    val typeFullName = Try(enumConstant.resolve()) match {
      case Success(resolvedDeclaration) =>
        resolvedTypeFullName(resolvedDeclaration.getType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve enum entry type for ${enumConstant.getNameAsString}")
        "ANY"
    }

    registerType(typeFullName)
  }

  def getReturnType(node: Resolvable[ResolvedMethodDeclaration]): String = {
    val typeFullName = Try(node.resolve().getReturnType) match {
      case Success(resolved) => resolvedTypeFullName(resolved)

      case Failure(_) =>
        logger.debug(s"Failed to resolve return type. Defaulting to ANY.")
        "ANY"
    }

    registerType(typeFullName)
  }

  def getTypeFullName(nameExpr: NameExpr): String = {
    val typeFullName = Try(nameExpr.resolve()) match {
      case Success(resolvedValueDeclaration) =>
        Try(resolvedTypeFullName(resolvedValueDeclaration.getType)).getOrElse {
          logger.debug(s"Failed to resolve type of ${resolvedValueDeclaration}. Falling back to name.")
          nameExpr.getNameAsString
        }

      case Failure(_) =>
        logger.debug(s"Failed to resolved type for nameExpr ${nameExpr.getNameAsString}. Falling back to name.")
        nameExpr.getNameAsString

    }

    registerType(typeFullName)
  }

  def getTypeFullName(thisExpr: ThisExpr): String = {
    val typeFullName = Try(thisExpr.resolve()) match {
      case Success(declaration) => resolvedTypeDeclFullName(declaration)

      case Failure(_) =>
        logger.debug(s"Failed to resolve type for `this` expr. Defaulting to ANY")
        "ANY"
    }

    registerType(typeFullName)
  }

  def getMethodLikeTypeFullName(methodLike: Resolvable[_ <: ResolvedMethodLikeDeclaration]): String = {
    val typeFullName = Try(methodLike.resolve()) match {
      case Success(declaration) => resolvedMethodLikeDeclFullName(declaration)

      case Failure(_) =>
        logger.debug(s"Failed to resolve type for method-like $methodLike. Defaulting to ANY")
        "ANY"
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
      case _                       => "ANY"
    }

    logger.debug(s"Processing type for literal ${literalExpr.getClass}: $typeFullName")
    registerType(typeFullName)
  }

  def getTypeFullName(invocation: ExplicitConstructorInvocationStmt): String = {
    val typeFullName = Try(invocation.resolve()) match {
      case Success(declaration) =>
        resolvedMethodLikeDeclFullName(declaration)

      case Failure(_) =>
        logger.debug(s"Could not resolve type for constructor invocation $invocation. Defaulting to ANY.")
        "ANY"
    }

    registerType(typeFullName)
  }

  def getTypeFullName(resolvedParam: ResolvedParameterDeclaration): String = {
    val typeFullName = resolvedTypeFullName(resolvedParam.getType)

    registerType(typeFullName)
  }

  def getTypeForExpression(expr: Expression): String = {
    val typeFullName = Try(expr.calculateResolvedType()) match {
      case Success(resolvedType) => resolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Could not resolve type for expr $expr")
        "ANY"
    }

    registerType(typeFullName)
  }

  def getInitializerType(variableDeclarator: VariableDeclarator): Option[String] = {
    variableDeclarator.getInitializer.toScala flatMap { initializer =>
      Try(initializer.calculateResolvedType()) match {
        case Success(resolvedType) =>
          Some(registerType(resolvedTypeFullName(resolvedType)))

        case Failure(_) =>
          logger.debug(s"Failed to resolve type for initializer ${initializer.toString}")
          None
      }
    }
  }

  def scopeType(scopeContext: ScopeContext, isSuper: Boolean = false): String = {
    scopeContext.typeDecl match {
      case Some(typ) =>
        if (isSuper) {
          val parentType =
            typ.inheritsFromTypeFullName.headOption
              .getOrElse("ANY")
          registerType(parentType)
        } else {
          registerType(typ.fullName)
        }

      case None => "ANY"
    }
  }
}

object TypeInfoProvider {
  def apply(global: Global): TypeInfoProvider = {
    new TypeInfoProvider(global)
  }

  def isAutocastType(typeName: String): Boolean = {
    NumericTypes.contains(typeName)
  }

  val NumericTypes = Set(
    "byte",
    "short",
    "int",
    "long",
    "float",
    "double",
    "char",
    "boolean",
    "java.lang.Byte",
    "java.lang.Short",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.Character",
    "java.lang.Boolean"
  )
}
