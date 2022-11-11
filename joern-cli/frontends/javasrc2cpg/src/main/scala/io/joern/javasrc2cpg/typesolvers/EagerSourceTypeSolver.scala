package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.symbolsolver.model.resolution.{SymbolReference, TypeSolver}
import io.joern.javasrc2cpg.JpAstWithMeta
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

class EagerSourceTypeSolver(asts: List[JpAstWithMeta]) extends TypeSolver {

  private val logger             = LoggerFactory.getLogger(this.getClass)
  private var parent: TypeSolver = _

  private val foundTypes: Map[String, SymbolReference[ResolvedReferenceTypeDeclaration]] = {
    val ret = asts
      .map(_.compilationUnit)
      .flatMap { cu =>
        cu.findAll(classOf[TypeDeclaration[_]])
          .asScala
          .map { typeDeclaration =>
            val name = typeDeclaration.getFullyQualifiedName.toScala match {
              case Some(fullyQualifiedName) => fullyQualifiedName
              case None =>
                val name = typeDeclaration.getNameAsString
                logger.error(s"Could not find fully qualified name for typeDecl $name")
                name
            }
            val resolvedSymbol = Try(
              SymbolReference.solved(JavaParserFacade.get(this).getTypeDeclaration(typeDeclaration)): SymbolReference[
                ResolvedReferenceTypeDeclaration
              ]
            ).getOrElse(SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration]))
            name -> resolvedSymbol
          }
          .toList
      }
      .toMap
    ret
  }

  override def getParent: TypeSolver = parent

  override def setParent(parent: TypeSolver): Unit = {
    if (parent == null) {
      logger.warn(s"Cannot set parent of type solver to null. setParent will be ignored.")
    } else if (this.parent != null) {
      logger.warn(s"Attempting to re-set type solver parent. setParent will be ignored.")
    } else if (parent == this) {
      logger.warn(s"Parent of TypeSolver cannot be itself. setParent will be ignored.")
    } else {
      this.parent = parent
    }
  }

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    foundTypes.getOrElse(name, SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration]))
  }
}

object EagerSourceTypeSolver {
  def apply(asts: List[JpAstWithMeta]): EagerSourceTypeSolver = {
    new EagerSourceTypeSolver(asts)
  }
}
