package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.model.resolution.{SymbolReference, TypeSolver}
import com.github.javaparser.resolution.UnsolvedSymbolException
import io.joern.javasrc2cpg.typesolvers.JdkJarTypeSolver._
import javassist.ClassPool

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.util.Using
import org.slf4j.LoggerFactory
import java.util.jar.JarFile
import java.io.IOException
import java.util.jar.JarEntry
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import io.joern.x2cpg.SourceFiles
import javassist.CtClass
import com.github.javaparser.symbolsolver.javassistmodel.JavassistFactory
import javassist.NotFoundException

class JdkJarTypeSolver private (jdkPath: String) extends TypeSolver {

  private val logger = LoggerFactory.getLogger(this.getClass())

  private var parent: Option[TypeSolver] = None
  private val classPool                  = new NonCachingClassPool()
  private val knownClassNames            = mutable.Map[String, String]()

  private type RefType = ResolvedReferenceTypeDeclaration

  override def getParent(): TypeSolver = parent.get

  override def setParent(parent: TypeSolver): Unit = {
    this.parent match {
      case None =>
        this.parent = Some(parent)

      case Some(_) =>
        throw new RuntimeException("JdkJarTypeSolver parent may only be set once")
    }
  }

  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    knownClassNames
      .get(name)
      .flatMap(lookupAndConvertClass)
      .getOrElse(SymbolReference.unsolved(classOf[RefType]))
  }

  override def solveType(name: String): ResolvedReferenceTypeDeclaration = {
    tryToSolveType(name) match {
      case symbolReference if symbolReference.isSolved() =>
        symbolReference.getCorrespondingDeclaration()

      case _ => throw new UnsolvedSymbolException(name)
    }
  }

  private def ctClassToRefType(ctClass: CtClass): RefType = {
    JavassistFactory.toTypeDeclaration(ctClass, getRoot())
  }

  private def refTypeToSymbolReference(refType: RefType): SymbolReference[RefType] = {
    SymbolReference.solved[RefType, RefType](refType)
  }

  private def lookupAndConvertClass(name: String): Option[SymbolReference[RefType]] = {
    Try(classPool.get(name)) match {
      case Success(ctClass) =>
        val refType      = ctClassToRefType(ctClass)
        val solvedSymbol = refTypeToSymbolReference(refType)
        Some(solvedSymbol)

      case Failure(_: NotFoundException) =>
        logger.error(s"BUG! Could not find class $name in class pool. This is not supposed to be possible!")
        None

      case Failure(e) =>
        logger.warn("Unexpected exception getting $name from class pool", e)
        None
    }
  }

  private def addJarToClassPool(path: String): Unit = {
    path match {
      case jarPath if jarPath.endsWith(".jar") =>
        classPool.appendClassPath(jarPath)

      case jmodPath =>
        classPool.appendClassPath(new JdkArchiveClassPath(jmodPath))
    }
  }

  private def withJars(jarPaths: Seq[String]): JdkJarTypeSolver = {
    jarPaths.foreach { jarPath =>
      Try(classPool.appendClassPath(new JdkArchiveClassPath(jarPath))) match {
        case Success(_) => registerKnownClassesForJar(jarPath)

        case Failure(e) =>
          logger.warn(s"Could not load jar at path $jarPath", e.getMessage())
      }
    }
    this
  }

  private def registerJarEntry(jarEntry: JarEntry): Unit = {
    val entryName = jarEntry.getName()

    if (!jarEntry.isDirectory && entryName.endsWith(ClassExtension)) {
      val javaParserName = convertEntryPathToJavaParserName(entryName)
      val classPoolName  = convertEntryPathToClassPoolName(entryName)

      // Avoid keeping 2 identical copies of the name.
      if (javaParserName == classPoolName) {
        knownClassNames.put(javaParserName, javaParserName)
      } else {
        knownClassNames.put(javaParserName, classPoolName)
      }
    }
  }

  private def registerKnownClassesForJar(jarPath: String): Unit = {
    try {
      Using(new JarFile(jarPath)) { jarFile =>
        jarFile
          .entries()
          .asIterator()
          .asScala
          .foreach(registerJarEntry)
      }
    } catch {
      case ioException: IOException =>
        logger.warn(s"Could register classes for jar/jmod at $jarPath", ioException.getMessage())
    }
  }
}

object JdkJarTypeSolver {
  val ClassExtension: String  = ".class"
  val JmodClassPrefix: String = "classes/"

  def fromJdkPath(jdkPath: String): JdkJarTypeSolver = {
    val jarPaths = SourceFiles.determine(jdkPath, Set(".jar", ".jmod"))
    new JdkJarTypeSolver(jdkPath).withJars(jarPaths)
  }

  /** Convert the JarEntry path into the qualified name format expected by JavaParser
    *
    * JarEntry format : foo/bar/Baz$Qux.class JavaParser format: foo.bar.Baz.Qux
    */
  def convertEntryPathToJavaParserName(entryPath: String): String = {
    convertEntryPathToClassPoolName(entryPath).replace('$', '.')
  }

  /** Convert the JarEntry path into the qualified name format expected by Javassist ClassPools
    *
    * JarEntry format : foo/bar/Baz$Qux.class ClassPool format: foo.bar.Baz$Qux
    */
  def convertEntryPathToClassPoolName(entryPath: String): String = {
    if (!entryPath.endsWith(ClassExtension)) {
      throw new IllegalArgumentException(s"The entry path should end with $ClassExtension")
    }
    entryPath
      .stripPrefix(JmodClassPrefix)
      .stripSuffix(ClassExtension)
      .replace('/', '.')
  }
}
