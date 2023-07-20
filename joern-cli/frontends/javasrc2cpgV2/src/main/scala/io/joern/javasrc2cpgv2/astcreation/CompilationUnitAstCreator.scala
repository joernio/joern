package io.joern.javasrc2cpgv2.astcreation

import com.github.javaparser.ast.{
  CompilationUnit, 
  ImportDeclaration,
  PackageDeclaration
}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewImport,
  NewNamespaceBlock
}
import io.joern.x2cpg.Ast

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional

trait CompilationUnitAstCreator { this: AstCreator =>

  private def namespaceForCompilationUnit(compilationUnit: CompilationUnit): NewNamespaceBlock = {
    compilationUnit
      .getPackageDeclaration()
      .toScala
      .map { packageDeclaration =>
        val name = packageDeclaration.getNameAsString()
        NewNamespaceBlock()
          .name(name)
          .fullName(s"${this.filename}:$name")
          .code(name)
          .lineNumber(line(packageDeclaration))
          .columnNumber(column(packageDeclaration))
          .filename(this.filename)
      }.getOrElse {
        globalNamespaceBlock()
      }
  }

  private def astForImportDeclaration(importDeclaration: ImportDeclaration): Ast = {
    val isWildcard = importDeclaration.isAsterisk
    ???
  }

  def astForCompilationUnit(compilationUnit: CompilationUnit): Ast = {
    val namespace = namespaceForCompilationUnit(compilationUnit)

    val importAsts = compilationUnit.getImports().asScala.map(astForImportDeclaration)
  }
}