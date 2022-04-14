package io.joern.javasrc2cpg.util

import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, ReferenceType}
import com.github.javaparser.ast.body.{EnumConstantDeclaration, TypeDeclaration, VariableDeclarator}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.nodeTypes.{NodeWithName, NodeWithType}
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.resolution.declarations._
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import io.joern.x2cpg.datastructures.Global
import io.joern.javasrc2cpg.util.TypeInfoProvider.{ImportInfo, UnresolvedTypeDefault}
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

class TypeInfoProvider(global: Global) {

  private val logger     = LoggerFactory.getLogger(this.getClass)
  private var importInfo = ImportInfo(Map.empty, None)

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map. Skip the `ANY` type, since this is created by default.
    */
  def registerType(typeName: String): String = {
    if (typeName != "ANY") {
      global.usedTypes.putIfAbsent(typeName, true)
    }
    typeName
  }

  private def simpleResolvedTypeFullName(resolvedType: ResolvedType): Option[String] = {
    if (resolvedType.isTypeVariable) {
      None
    } else {
      Some(resolvedType.describe())
    }
  }

  private def buildTypeString(packageName: String, className: String): String = {
    val dollaredClass = className.replaceAll("\\.", "\\$")
    if (packageName.nonEmpty) {
      s"$packageName.$dollaredClass"
    } else {
      dollaredClass
    }
  }

  private def extractNullableName(tryName: Try[String]): String = {
    tryName match {
      case Success(null) => ""

      case Success(name) => name

      case _ => ""
    }
  }

  private def resolvedTypeDeclFullName(declaration: ResolvedTypeDeclaration): String = {
    val packageName = extractNullableName(Try(declaration.getPackageName))
    val className   = extractNullableName(Try(declaration.getClassName))
    buildTypeString(packageName, className)
  }

  private def resolvedMethodLikeDeclFullName(declaration: ResolvedMethodLikeDeclaration): String = {
    val packageName = Try(declaration.getPackageName).getOrElse("")
    val className   = Try(declaration.getClassName).getOrElse(declaration.getName)

    buildTypeString(packageName, className)
  }

  private def resolvedReferenceTypeFullName(resolvedType: ResolvedReferenceType): Option[String] = {
    resolvedType.getTypeDeclaration.toScala match {
      case Some(typeDeclaration) => Some(resolvedTypeDeclFullName(typeDeclaration))

      case None =>
        simpleResolvedTypeFullName(resolvedType)
    }
  }

  private def resolvedTypeFullName(resolvedType: ResolvedType): Option[String] = {
    resolvedType match {
      case resolvedReferenceType: ResolvedReferenceType => resolvedReferenceTypeFullName(resolvedReferenceType)

      case _ => simpleResolvedTypeFullName(resolvedType)
    }
  }

  def getResolvedTypeFullName(resolvedType: ResolvedType): Option[String] = {
    resolvedTypeFullName(resolvedType).map(registerType)
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

  def getTypeFullName(node: NodeWithType[_, _ <: Resolvable[ResolvedType]]): Option[String] = {
    val typeFullName = Try(node.getType.resolve()) match {
      case Success(resolvedType: ResolvedParameterDeclaration) =>
        getTypeFullName(resolvedType)

      case Success(resolvedType: ResolvedReferenceType) =>
        resolvedReferenceTypeFullName(resolvedType)

      case Success(resolvedType: ResolvedType) => simpleResolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Resolving type ${node.getTypeAsString} failed. Falling back to import default.")
        importInfo.getType(node.getTypeAsString)
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(typ: ClassOrInterfaceType): Option[String] = {
    val typeFullName = Try(typ.resolve) match {
      case Success(resolvedType) => resolvedReferenceTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve class type ${typ.getNameAsString}. Falling back to imports info.")
        importInfo.getType(typ.getNameAsString)
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(annotationExpr: AnnotationExpr): Option[String] = {
    val typeFullName = Try(annotationExpr.resolve()) match {
      case Success(resolvedType) => Some(resolvedTypeDeclFullName(resolvedType))

      case Failure(_) =>
        logger.debug(
          s"Failed to resolve annotation type ${annotationExpr.getNameAsString}. Falling back to imports info."
        )
        importInfo.getType(annotationExpr.getNameAsString)
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(enumConstant: EnumConstantDeclaration): Option[String] = {
    val typeFullName = Try(enumConstant.resolve()) match {
      case Success(resolvedDeclaration) =>
        resolvedTypeFullName(resolvedDeclaration.getType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve enum entry type for ${enumConstant.getNameAsString}")
        None
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(referenceType: ReferenceType): Option[String] = {
    val typeFullName = Try(referenceType.resolve()).toOption.flatMap(resolvedTypeFullName)

    typeFullName.map(registerType)
  }

  def getReturnType(node: Resolvable[ResolvedMethodDeclaration]): Option[String] = {
    val typeFullName = Try(node.resolve().getReturnType) match {
      case Success(resolved) => resolvedTypeFullName(resolved)

      case Failure(_) =>
        logger.debug(s"Failed to resolve return type.")
        None
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(nameExpr: NameExpr): Option[String] = {
    val typeFullName = Try(nameExpr.calculateResolvedType()) match {
      case Success(resolvedValueDeclaration) => resolvedTypeFullName(resolvedValueDeclaration)

      case Failure(_) =>
        logger.debug(s"Failed to resolved type for nameExpr ${nameExpr.getNameAsString}.")
        None

    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(thisExpr: ThisExpr): Option[String] = {
    val typeFullName =
      Try(thisExpr.resolve()).toOption
        .map(typeDecl => resolvedTypeDeclFullName(typeDecl))

    typeFullName.map(registerType)
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
      case _                       => UnresolvedTypeDefault
    }

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

  def getTypeFullName(resolvedParam: ResolvedParameterDeclaration): Option[String] = {
    val typeFullName = resolvedTypeFullName(resolvedParam.getType)
    val arraySuffix = if (resolvedParam.isVariadic) {
      "[]"
    } else {
      ""
    }

    typeFullName.map(_ ++ arraySuffix).map(registerType)
  }

  def getTypeForExpression(expr: Expression): Option[String] = {
    val typeFullName = Try(expr.calculateResolvedType()) match {
      case Success(resolvedType) => resolvedTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Could not resolve type for expr $expr")
        None
    }

    typeFullName.map(registerType)
  }

  def getInitializerType(variableDeclarator: VariableDeclarator): Option[String] = {
    val typeFullName = variableDeclarator.getInitializer.toScala flatMap { initializer =>
      Try(initializer.calculateResolvedType()) match {
        case Success(resolvedType) => resolvedTypeFullName(resolvedType)

        case Failure(_) =>
          logger.debug(s"Failed to resolve type for initializer ${initializer.toString}")
          None
      }
    }

    typeFullName.map(registerType)
  }

  def scopeType(scopeStack: Scope, isSuper: Boolean = false): String = {
    scopeStack.getEnclosingTypeDecl match {
      case Some(typ) if isSuper =>
        val parentType = typ.inheritsFromTypeFullName.headOption.getOrElse(UnresolvedTypeDefault)
        registerType(parentType)
      case Some(typ) =>
        registerType(typ.fullName)
      case None => UnresolvedTypeDefault
    }
  }

  def registerImports(imports: List[ImportDeclaration]): Unit = {
    val (asteriskImports, specificImports) = imports.partition(_.isAsterisk)
    val identifierMap = specificImports.map { importDecl =>
      importDecl.getName.getIdentifier -> importDecl.getNameAsString
    }.toMap

    val wildcardImport = asteriskImports match {
      case imp :: Nil => Some(imp.getNameAsString)

      case _ => None
    }

    importInfo = ImportInfo(identifierMap, wildcardImport)
  }
}

object TypeInfoProvider {
  def apply(global: Global): TypeInfoProvider = {
    new TypeInfoProvider(global)
  }

  def isAutocastType(typeName: String): Boolean = {
    NumericTypes.contains(typeName)
  }

  object TypeConstants {
    val Byte: String    = "byte"
    val Short: String   = "short"
    val Int: String     = "int"
    val Long: String    = "long"
    val Float: String   = "float"
    val Double: String  = "double"
    val Char: String    = "char"
    val Boolean: String = "boolean"
    val Object: String  = "java.lang.Object"
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

  val UnresolvedTypeDefault = "ANY"

  case class ImportInfo(identifierMap: Map[String, String], wildcardImport: Option[String]) {
    def getType(name: String): Option[String] = {
      identifierMap.get(name).orElse {
        if (NumericTypes.contains(name)) {
          Some(name)
        } else {
          wildcardImport.map(wc => s"$wc.$name")
        }
      }
    }
  }
}
