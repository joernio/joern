package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.TypeSolver
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.model.SymbolReference

/** A type solver which will never return a solved type. Used as a placeholder for creating Contexts where a type solver
  * is required, but not used.
  */
class EmptyTypeSolver extends TypeSolver {

  private var parent: TypeSolver = scala.compiletime.uninitialized

  override def getParent: TypeSolver = {
    parent
  }

  override def setParent(parent: TypeSolver): Unit = {
    this.parent = parent
  }

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    SymbolReference.unsolved()
  }
}
