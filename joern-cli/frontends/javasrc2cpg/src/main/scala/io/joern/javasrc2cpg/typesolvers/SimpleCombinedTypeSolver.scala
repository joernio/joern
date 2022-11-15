package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.UnsolvedSymbolException
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.github.javaparser.symbolsolver.model.resolution.{SymbolReference, TypeSolver}
import com.google.common.cache.{CacheBuilder, LoadingCache}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional

class SimpleCombinedTypeSolver extends TypeSolver {

  private val logger                                       = LoggerFactory.getLogger(this.getClass)
  private var parent: TypeSolver                           = _
  private val typeSolvers: mutable.ArrayBuffer[TypeSolver] = mutable.ArrayBuffer()
  private val typeCache = new GuavaCache(
    CacheBuilder.newBuilder().build[String, SymbolReference[ResolvedReferenceTypeDeclaration]]()
  )

  def add(typeSolver: TypeSolver): Unit = {
    typeSolvers.append(typeSolver)
    typeSolver.setParent(this)
  }

  def prepend(typeSolver: TypeSolver): Unit = {
    typeSolvers.prepend(typeSolver)
    typeSolver.setParent(this)
  }

  private def unsolved: SymbolReference[ResolvedReferenceTypeDeclaration] =
    SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    typeCache.get(name).toScala match {
      case Some(result) => result

      case None =>
        // Use an iterator here so that the map is only applied until a solved result is found.
        val result = typeSolvers.iterator
          .map { typeSolver =>
            try {
              typeSolver.tryToSolveType(name): SymbolReference[ResolvedReferenceTypeDeclaration]
            } catch {
              case _: UnsolvedSymbolException  => unsolved
              case _: StackOverflowError       => unsolved
              case _: IllegalArgumentException =>
                // RecordDeclarations aren't handled by JavaParser yet
                unsolved
              case unhandled =>
                logger.warn("Caught unhandled exception", unhandled)
                unsolved
            }
          }
          .find(_.isSolved)
          .getOrElse(unsolved)
        typeCache.put(name, result)
        result
    }
  }

  override def solveType(name: String): ResolvedReferenceTypeDeclaration = {
    val result = tryToSolveType(name)
    if (result.isSolved)
      result.getCorrespondingDeclaration
    else
      throw new UnsolvedSymbolException(name)
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

}
