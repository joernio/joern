package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.UnsolvedSymbolException
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.github.javaparser.resolution.TypeSolver
import com.github.javaparser.resolution.model.SymbolReference
import com.google.common.cache.CacheBuilder
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional

class SimpleCombinedTypeSolver extends TypeSolver {

  private val logger             = LoggerFactory.getLogger(this.getClass)
  private var parent: TypeSolver = _
  // Ideally all types would be cached in the SimpleCombinedTypeSolver to avoid unnecessary unresolved types
  // from being cached. The EagerSourceTypeSolver preloads all types, however, so separating caching and
  // non-caching solvers avoids caching types twice.
  private val cachingTypeSolvers: mutable.ArrayBuffer[TypeSolver]    = mutable.ArrayBuffer()
  private val nonCachingTypeSolvers: mutable.ArrayBuffer[TypeSolver] = mutable.ArrayBuffer()

  private val typeCache = new GuavaCache(
    CacheBuilder.newBuilder().build[String, SymbolReference[ResolvedReferenceTypeDeclaration]]()
  )

  def addCachingTypeSolver(typeSolver: TypeSolver): Unit = {
    cachingTypeSolvers.append(typeSolver)
    typeSolver.setParent(this)
  }

  def addNonCachingTypeSolver(typeSolver: TypeSolver): Unit = {
    nonCachingTypeSolvers.prepend(typeSolver)
    typeSolver.setParent(this)
  }

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    typeCache.get(name).toScala match {
      case Some(result) => result

      case None =>
        findSolvedTypeWithSolvers(cachingTypeSolvers, name)
          .getOrElse {
            val result = findSolvedTypeWithSolvers(nonCachingTypeSolvers, name).getOrElse(SymbolReference.unsolved())
            typeCache.put(name, result)
            result
          }
    }
  }

  private def findSolvedTypeWithSolvers(
    typeSolvers: mutable.ArrayBuffer[TypeSolver],
    className: String
  ): Option[SymbolReference[ResolvedReferenceTypeDeclaration]] = {
    typeSolvers.iterator
      .map { typeSolver =>
        try {
          val result = typeSolver.tryToSolveType(className): SymbolReference[ResolvedReferenceTypeDeclaration]
          Option.when(result.isSolved())(result)
        } catch {
          case _: UnsolvedSymbolException  => None
          case _: StackOverflowError       => None
          case _: IllegalArgumentException =>
            // RecordDeclarations aren't handled by JavaParser yet
            None
          case unhandled: Throwable =>
            logger.warn("Caught unhandled exception", unhandled)
            None
        }
      }
      .collectFirst { case Some(symbolReference) =>
        symbolReference
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
