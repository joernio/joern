package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.types.ResolvedReferenceType

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
