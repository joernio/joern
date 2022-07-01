package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.types.ResolvedReferenceType
import io.joern.javasrc2cpg.util.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewFieldIdentifier}

import scala.collection.mutable
import scala.util.Try
import scala.jdk.CollectionConverters._

object Util {
  def composeMethodFullName(typeDeclFullName: String, name: String, signature: String): String = {
    s"$typeDeclFullName.$name:$signature"
  }

  def getAllParents(typeDecl: ResolvedReferenceTypeDeclaration): mutable.ArrayBuffer[ResolvedReferenceType] = {
    val result = mutable.ArrayBuffer.empty[ResolvedReferenceType]

    if (!typeDecl.isJavaLangObject) {
      typeDecl.getAncestors(true).asScala.foreach { ancestor =>
        result.append(ancestor)
        getAllParents(ancestor, result)
      }
    }

    result
  }

  def composeMethodLikeSignature(returnType: String, parameterTypes: collection.Seq[String]): String = {
    s"$returnType(${parameterTypes.mkString(",")})"
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

  // TODO: Move to x2cpg once order setting is no longer required
  // TODO: Switch to this for all operators. Some were moved in https://github.com/joernio/joern/pull/1437,
  // but that PR was long enough as is.
  def operatorCallNode(
    name: String,
    code: String,
    typeFullName: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewCall = {
    NewCall()
      .name(name)
      .methodFullName(name)
      .code(code)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(typeFullName.getOrElse(TypeConstants.UnresolvedType))
      .lineNumber(line)
      .columnNumber(column)
  }

  def fieldIdentifierNode(
    name: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewFieldIdentifier = {
    NewFieldIdentifier()
      .canonicalName(name)
      .code(name)
      .lineNumber(line)
      .columnNumber(column)
  }
}
