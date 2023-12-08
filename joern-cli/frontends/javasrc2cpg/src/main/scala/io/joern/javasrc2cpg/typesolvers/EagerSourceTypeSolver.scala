package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.resolution.TypeSolver
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.model.SymbolReference
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import io.joern.javasrc2cpg.util.SourceParser
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

class EagerSourceTypeSolver(
  filenames: Array[String],
  sourceParser: SourceParser,
  combinedTypeSolver: SimpleCombinedTypeSolver,
  symbolSolver: JavaSymbolSolver
) extends TypeSolver {

  private val logger             = LoggerFactory.getLogger(this.getClass)
  private var parent: TypeSolver = _

  private val foundTypes: Map[String, SymbolReference[ResolvedReferenceTypeDeclaration]] = {
    filenames
      .flatMap(sourceParser.parseTypesFile)
      .flatMap { cu =>
        symbolSolver.inject(cu)
        cu.findAll(classOf[TypeDeclaration[_]])
          .asScala
          .map { typeDeclaration =>
            val name = typeDeclaration.getFullyQualifiedName.toScala match {
              case Some(fullyQualifiedName) => fullyQualifiedName
              case None =>
                val name = typeDeclaration.getNameAsString
                // Local classes aren't expected to have a fully qualified name
                if (typeDeclaration.isTopLevelType() || typeDeclaration.isNestedType()) {
                  logger.warn(s"Could not find fully qualified name for typeDecl $name")
                }
                name
            }
            TypeSizeReducer.simplifyType(typeDeclaration)
            val resolvedSymbol = Try(
              SymbolReference.solved(
                JavaParserFacade.get(combinedTypeSolver).getTypeDeclaration(typeDeclaration)
              ): SymbolReference[ResolvedReferenceTypeDeclaration]
            ).getOrElse(SymbolReference.unsolved())
            name -> resolvedSymbol
          }
          .toList
      }
      .toMap
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
    foundTypes.getOrElse(name, SymbolReference.unsolved())
  }
}

object EagerSourceTypeSolver {
  def apply(
    filenames: Array[String],
    sourceParser: SourceParser,
    combinedTypeSolver: SimpleCombinedTypeSolver,
    symbolSolver: JavaSymbolSolver
  ): EagerSourceTypeSolver = {
    new EagerSourceTypeSolver(filenames, sourceParser, combinedTypeSolver, symbolSolver)
  }
}
