package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.ast.stmt.BlockStmt

import scala.jdk.CollectionConverters.*

object TypeSizeReducer {
  def simplifyType(typeDeclaration: TypeDeclaration[?]): Unit = {
    typeDeclaration
      .getMethods()
      .asScala
      .filter(method => method.getBody().isPresent())
      .foreach { method =>
        method.setBody(new BlockStmt())
      }
  }
}
