package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ConstructorDeclaration, EnumConstantDeclaration, MethodDeclaration, TypeDeclaration, VariableDeclarator}
import com.github.javaparser.ast.expr.{BooleanLiteralExpr, CharLiteralExpr, DoubleLiteralExpr, Expression, IntegerLiteralExpr, LiteralExpr, LongLiteralExpr, MethodCallExpr, NameExpr, NullLiteralExpr, StringLiteralExpr, TextBlockLiteralExpr}
import com.github.javaparser.ast.nodeTypes.NodeWithType
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.resolution.declarations.{ResolvedMethodDeclaration, ResolvedMethodLikeDeclaration, ResolvedReferenceTypeDeclaration, ResolvedTypeDeclaration}
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import org.slf4j.LoggerFactory

import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

class TypeInfoProvider(global: Global) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Add `typeName` to a global map and return it. The
    * map is later passed to a pass that creates TYPE
    * nodes for each key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }


  private def simpleResolvedTypeFullName(resolvedType: ResolvedType): String = {
    resolvedType.describe()
  }

  private def resolvedTypeDeclFullName(declaration: ResolvedTypeDeclaration): String = {
    declaration.getPackageName ++ declaration.getClassName.replaceAll(".", "$")
  }

  private def resolvedMethodLikeDeclFullName(declaration: ResolvedMethodLikeDeclaration): String = {
    declaration.getPackageName ++ declaration.getClassName.replaceAll(".", "$")
  }

  private def resolvedReferenceTypeFullName(resolvedType: ResolvedReferenceType): String = {
    resolvedType.getTypeDeclaration.toScala match {
      case Some (typeDeclaration) => resolvedTypeDeclFullName(typeDeclaration)

      case None =>
        simpleResolvedTypeFullName(resolvedType)
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

  def getTypeFullName(constructorDeclaration: ConstructorDeclaration): String = {
    val typeFullName = Try(constructorDeclaration.resolve()) match {
      case Success(resolvedDeclaration) =>
        resolvedTypeDeclFullName(resolvedDeclaration.declaringType())

      case Failure(_) =>
        logger.info(s"Failed to resolve constructor declaration type for ${constructorDeclaration.toString}")
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


//  def getTypeFullName(expr: _ <: Resolvable[ResolvedTypeDeclaration]): String = {
//    val typeFullName = Try(expr.resolve()) match {
//      case Success(resolvedDecl) =>
//        resolvedTypeDeclFullName(resolvedDecl)
//
//      case Failure(_) =>
//        logger.info(s"Failed to resolve type for expr ${expr.toString}. Falling back to <empty>.")
//        "<empty>"
//    }
//
//    registerType(typeFullName)
//  }

  def getTypeFullName(callExpr: MethodCallExpr): String = {
    val typeFullName = Try(callExpr.resolve()) match {
      case Success(declaration) => resolvedMethodLikeDeclFullName(declaration)

      case Failure(_) =>
        logger.info(s"Failed to resolve type for call ${callExpr.getName}. Falling back to <empty>.")
        "<empty>"
    }

    registerType(typeFullName)
  }

  def getTypeFullName(literalExpr: LiteralExpr): String = {
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

  def getInitializerType(variableDeclarator: VariableDeclarator): String = {
    val typeFullName = variableDeclarator.getInitializer.toScala match {
      case None => "<empty>"

      case Some(initializer) =>
        Try(initializer.calculateResolvedType()) match {
          case Success(resolvedType) => resolvedTypeFullName(resolvedType)

          case Failure(_) =>
            logger.info(s"Failed to resolve type for initializer ${initializer.toString}")
            "<empty>"
        }
    }

    registerType(typeFullName)
  }
}

object TypeInfoProvider {
  def apply(global: Global): TypeInfoProvider = {
    new TypeInfoProvider(global)
  }
}
