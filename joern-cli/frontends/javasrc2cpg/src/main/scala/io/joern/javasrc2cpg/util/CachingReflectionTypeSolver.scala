package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.github.javaparser.symbolsolver.model.resolution.SymbolReference
import com.github.javaparser.symbolsolver.reflectionmodel.ReflectionFactory
import com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver
import com.google.common.cache.CacheBuilder

import scala.jdk.CollectionConverters._

// This class caches the `loadClass` operation in order to avoid
// duplicate loading of the same class over and over again.
class CachingReflectionTypeSolver extends ReflectionTypeSolver {
  private val cache = new GuavaCache(
    CacheBuilder.newBuilder().build[String, SymbolReference[ResolvedReferenceTypeDeclaration]]()
  )
  private val classLoader = classOf[CachingReflectionTypeSolver].getClassLoader

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    if (filterName(name)) {
      // Some implementations could return null when the class was loaded through the bootstrap classloader
      // see https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getClassLoader--
      if (classLoader == null) {
        throw new RuntimeException(
          "The ClassLoaderTypeSolver has been probably loaded through the bootstrap class loader. This usage is not supported by the JavaSymbolSolver"
        )
      }

      // The separate query and store on the cache may lead to some duplicate work
      // if two separate threads query, find there is no entry and start calculating it.
      // But this is ok since they will end up with the same result and writing the same
      // results twice to the cache wont hurt us.
      val symbolReferenceOption = cache.get(name)
      if (symbolReferenceOption.isPresent) {
        symbolReferenceOption.get()
      } else {
        try {
          val clazz = classLoader.loadClass(name)
          val symbolReference =
            SymbolReference.solved[ResolvedReferenceTypeDeclaration, ResolvedReferenceTypeDeclaration](
              ReflectionFactory.typeDeclarationFor(clazz, getRoot)
            )
          cache.put(name, symbolReference)
          symbolReference
        } catch {
          case e: NoClassDefFoundError =>
            // We can safely ignore this one because it is triggered when there are package names which are almost the
            // same as class name, with the exclusion of the case.
            // For example:
            // java.lang.NoClassDefFoundError: com/github/javaparser/printer/ConcreteSyntaxModel
            // (wrong name: com/github/javaparser/printer/concretesyntaxmodel)
            // note that this exception seems to be thrown only on certain platform (mac yes, linux no)
            SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])
          case e: ClassNotFoundException =>
            // it could be an inner class
            val symbolReference = innerClassLookup(name)
            cache.put(name, symbolReference)
            symbolReference
        }
      }
    } else {
      SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])
    }
  }

  private def innerClassLookup(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    val lastDot: Int = name.lastIndexOf('.')
    if (lastDot == -(1)) {
      SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])
    } else {
      val parentName: String                                        = name.substring(0, lastDot)
      val childName: String                                         = name.substring(lastDot + 1)
      val parent: SymbolReference[ResolvedReferenceTypeDeclaration] = tryToSolveType(parentName)
      if (parent.isSolved) {
        val innerClass = parent.getCorrespondingDeclaration.internalTypes.asScala.find(_.getName == childName)
        innerClass
          .map(SymbolReference.solved[ResolvedReferenceTypeDeclaration, ResolvedReferenceTypeDeclaration])
          .getOrElse(SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration]))
      } else {
        SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])
      }
    }
  }

}
