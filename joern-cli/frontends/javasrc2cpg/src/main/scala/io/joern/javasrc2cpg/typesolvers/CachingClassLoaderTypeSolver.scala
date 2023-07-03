package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.github.javaparser.symbolsolver.model.resolution.SymbolReference
import com.github.javaparser.symbolsolver.reflectionmodel.ReflectionFactory
import com.github.javaparser.symbolsolver.resolution.typesolvers.ClassLoaderTypeSolver
import com.google.common.cache.CacheBuilder
import io.joern.javasrc2cpg.typesolvers.CachingClassLoaderTypeSolver.NoResult

import scala.jdk.CollectionConverters._
import better.files.File
import java.net.URLClassLoader
import java.security.AccessControlContext
import org.slf4j.LoggerFactory
import io.joern.x2cpg.SourceFiles

// This class caches the `loadClass` operation in order to avoid
// duplicate loading of the same class over and over again.
class CachingClassLoaderTypeSolver(classLoader: ClassLoader) extends ClassLoaderTypeSolver(classLoader) {
  private val cache = new GuavaCache(
    CacheBuilder.newBuilder().build[String, SymbolReference[ResolvedReferenceTypeDeclaration]]()
  )

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
          if (clazz == classOf[NoResult]) throw new ClassNotFoundException(name)
          val symbolReference =
            SymbolReference.solved[ResolvedReferenceTypeDeclaration, ResolvedReferenceTypeDeclaration](
              ReflectionFactory.typeDeclarationFor(clazz, getRoot)
            )
          cache.put(name, symbolReference)
          symbolReference
        } catch {
          case _: NoClassDefFoundError =>
            // We can safely ignore this one because it is triggered when there are package names which are almost the
            // same as class name, with the exclusion of the case.
            // For example:
            // java.lang.NoClassDefFoundError: com/github/javaparser/printer/ConcreteSyntaxModel
            // (wrong name: com/github/javaparser/printer/concretesyntaxmodel)
            // note that this exception seems to be thrown only on certain platform (mac yes, linux no)
            SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])
          case _: ClassNotFoundException =>
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
    if (lastDot == -1) {
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

object CachingClassLoaderTypeSolver {
  private[CachingClassLoaderTypeSolver] class NoResult

  /** Use the NullClassLoader which "finds" the class NoResult
    * to avoid falling back to system class loader
    */
  private class NullClassLoader extends ClassLoader {
    override def loadClass(name: String): Class[_] = {
      throw new ClassNotFoundException(name)
    }

    override def loadClass(name: String, resolve: Boolean): Class[_] = {
      throw new ClassNotFoundException(name)
    }

    override def findClass(name: String): Class[_] = {
      throw new ClassNotFoundException(name)
    }

    override def findClass(moduleName: String, name: String): Class[_] = {
      throw new ClassNotFoundException(s"$moduleName:$name")
    }

    override def findLibrary(name: String): String = {
      throw new ClassNotFoundException(name)
    }
  }

  private val logger = LoggerFactory.getLogger(this.getClass)
  def getClassLoaderTypeSolver(jdkPath: Option[String]): Option[ClassLoaderTypeSolver] = {
    val classLoader = jdkPath.map(File(_)) match {
      case None => Some(classOf[ClassLoaderTypeSolver].getClassLoader())

      case Some(file) if file.exists =>
        val jars = SourceFiles.determine(file.canonicalPath, Set(".jar", ".jmod")).map(File(_).url).toArray
        Some(new URLClassLoader("CL", jars, new NullClassLoader()))

      case Some(file) =>
        logger.error(s"Invalid JDK path ${file.pathAsString}! Type information will be missing.")
        None
    }

    classLoader.map(new CachingClassLoaderTypeSolver(_))
  }
}
