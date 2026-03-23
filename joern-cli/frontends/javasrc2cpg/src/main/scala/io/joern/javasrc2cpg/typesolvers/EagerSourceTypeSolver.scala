package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.ast.modules.{ModuleDeclaration, ModuleExportsDirective}
import com.github.javaparser.resolution.TypeSolver
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.model.SymbolReference
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import io.joern.javasrc2cpg.util.SourceParser
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

class EagerSourceTypeSolver(
  sourceParser: SourceParser,
  combinedTypeSolver: SimpleCombinedTypeSolver,
  symbolSolver: JavaSymbolSolver,
  enableVerboseTypeLogging: Boolean
) extends TypeSolver {

  private val ModuleInfoFileName: String = "module-info.java"

  private val logger             = LoggerFactory.getLogger(this.getClass)
  private var parent: TypeSolver = scala.compiletime.uninitialized

  private val moduleExportedPackages: Map[String, List[String]] = {
    sourceParser.relativeFilenames
      .filter(_.endsWith(ModuleInfoFileName))
      .flatMap(sourceParser.parseTypesFile)
      .flatMap { cu =>
        cu.findAll(classOf[ModuleDeclaration])
          .asScala
          .map { moduleDeclaration =>
            val exports = moduleDeclaration.getDirectives.asScala.collect {
              case exportsDirective: ModuleExportsDirective => exportsDirective.getNameAsString
            }

            moduleDeclaration.getNameAsString -> exports.toList
          }
      }
      .toMap
  }

  private val foundTypes: Map[String, SymbolReference[ResolvedReferenceTypeDeclaration]] = {
    val result = sourceParser.relativeFilenames
      .flatMap(sourceParser.parseTypesFile)
      .flatMap { cu =>
        symbolSolver.inject(cu)
        cu.findAll(classOf[TypeDeclaration[?]])
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

    if (enableVerboseTypeLogging) {
      logger.debug(("EagerSourceTypeSolver found following types:" :: result.toList.sortBy(_._1).map {
        case (name, resolveResult) =>
          s"$name isSolved=${resolveResult.isSolved}"
      }).mkString(s"${System.lineSeparator()} - "))
    }

    result
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

  override def tryToSolveTypeInModule(
    qualifiedModuleName: String,
    simpleTypeName: String
  ): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    moduleExportedPackages
      .get(qualifiedModuleName)
      .flatMap { exportedPackages =>
        exportedPackages.iterator
          .map(packageName => tryToSolveType(s"$packageName.$simpleTypeName"))
          .collectFirst { case symbolReference if symbolReference.isSolved => symbolReference }
      }
      .getOrElse(SymbolReference.unsolved())
  }
}

object EagerSourceTypeSolver {
  def apply(
    sourceParser: SourceParser,
    combinedTypeSolver: SimpleCombinedTypeSolver,
    symbolSolver: JavaSymbolSolver,
    enableVerboseTypeLogging: Boolean
  ): EagerSourceTypeSolver = {
    new EagerSourceTypeSolver(sourceParser, combinedTypeSolver, symbolSolver, enableVerboseTypeLogging)
  }
}
