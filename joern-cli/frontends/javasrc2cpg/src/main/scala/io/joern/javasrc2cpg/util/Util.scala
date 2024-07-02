package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.types.ResolvedReferenceType
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewFieldIdentifier, NewMember}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

object Util {

  private val logger = LoggerFactory.getLogger(this.getClass)
  def composeMethodFullName(typeDeclFullName: String, name: String, signature: String): String = {
    s"$typeDeclFullName.$name:$signature"
  }

  def safeGetAncestors(typeDecl: ResolvedReferenceTypeDeclaration): Seq[ResolvedReferenceType] = {
    Try(typeDecl.getAncestors(true)) match {
      case Success(ancestors) => ancestors.asScala.filterNot(_ == typeDecl).toSeq

      case Failure(exception) =>
        logger.debug(s"Failed to get direct parents for typeDecl ${typeDecl.getQualifiedName}", exception)
        Seq.empty
    }
  }

  def getAllParents(typeDecl: ResolvedReferenceTypeDeclaration): mutable.ArrayBuffer[ResolvedReferenceType] = {
    val result = mutable.ArrayBuffer.empty[ResolvedReferenceType]

    if (!typeDecl.isJavaLangObject) {
      safeGetAncestors(typeDecl).filter(_.getQualifiedName != typeDecl.getQualifiedName).foreach { ancestor =>
        result.append(ancestor)
        getAllParents(ancestor, result)
      }
    }

    result
  }

  def composeMethodLikeSignature(returnType: String, parameterTypes: collection.Seq[String]): String = {
    s"$returnType(${parameterTypes.mkString(",")})"
  }

  def composeUnresolvedSignature(paramCount: Int): String = {
    s"${Defines.UnresolvedSignature}($paramCount)"
  }

  def stripGenericTypes(typeName: String): String = {
    if (typeName.startsWith(Defines.UnresolvedNamespace)) {
      logger.warn(s"stripGenericTypes should not be used for javasrc2cpg type $typeName")
      typeName.head +: takeUntilGenericStart(typeName.tail)
    } else {
      takeUntilGenericStart(typeName)
    }
  }

  private def takeUntilGenericStart(typeName: String): String = {
    typeName.takeWhile(char => char != '<')
  }

  private def getAllParents(typ: ResolvedReferenceType, result: mutable.ArrayBuffer[ResolvedReferenceType]): Unit = {
    if (typ.isJavaLangObject) {
      Iterable.empty
    } else {
      Try(typ.getDirectAncestors).map(_.asScala).getOrElse(Nil).foreach { ancestor =>
        result.append(ancestor)
        getAllParents(ancestor, result)
      }
    }
  }
}
