package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.types.ResolvedReferenceType
import io.joern.javasrc2cpg.util.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewFieldIdentifier, NewMember}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

object Util {

  private val logger = LoggerFactory.getLogger(this.getClass)
  def composeMethodFullName(typeDeclFullName: String, name: String, signature: String): String = {
    s"$typeDeclFullName.$name:$signature"
  }

  def safeGetAncestors(typeDecl: ResolvedReferenceTypeDeclaration): Seq[ResolvedReferenceType] = {
    Try(typeDecl.getAncestors(true)) match {
      case Success(ancestors) => ancestors.asScala.toSeq

      case Failure(exception) =>
        logger.warn(s"Failed to get direct parents for typeDecl ${typeDecl.getQualifiedName}", exception)
        Seq.empty
    }
  }

  def getAllParents(typeDecl: ResolvedReferenceTypeDeclaration): mutable.ArrayBuffer[ResolvedReferenceType] = {
    val result = mutable.ArrayBuffer.empty[ResolvedReferenceType]

    if (!typeDecl.isJavaLangObject) {
      safeGetAncestors(typeDecl).foreach { ancestor =>
        result.append(ancestor)
        getAllParents(ancestor, result)
      }
    }

    result
  }

  /** Creates an AST that represents a single member node
    */
  def memberNode(
    name: String,
    code: String,
    maybeTypeFullName: Option[String],
    lineNo: Option[Integer],
    columnNo: Option[Integer]
  ): NewMember = {
    val typeFullName = maybeTypeFullName.getOrElse("ANY")
    val memberNode = NewMember()
      .name(name)
      .code(code)
      .lineNumber(lineNo)
      .columnNumber(columnNo)
      .typeFullName(typeFullName)
    memberNode
  }

  def composeMethodLikeSignature(returnType: String, parameterTypes: collection.Seq[String]): String = {
    s"$returnType(${parameterTypes.mkString(",")})"
  }

  def composeUnresolvedSignature(paramCount: Int): String = {
    s"${Defines.UnresolvedSignature}($paramCount)"
  }

  def rootCode(ast: Seq[Ast]): String = {
    ast.headOption.flatMap(_.root).flatMap(_.properties.get(PropertyNames.CODE).map(_.toString)).getOrElse("")
  }

  def rootType(ast: Ast): Option[String] = {
    ast.root.flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME).map(_.toString))
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
