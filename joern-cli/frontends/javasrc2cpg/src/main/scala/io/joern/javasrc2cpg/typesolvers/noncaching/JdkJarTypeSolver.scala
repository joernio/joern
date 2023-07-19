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
import javassist.ClassPath

class JdkJarTypeSolver private (jdkPath: String) extends TypeSolver {

  private val logger = LoggerFactory.getLogger(this.getClass())

  private var parent: Option[TypeSolver] = None
  private val classPool                  = initClassPool

  private def initClassPool: NonCachingClassPool = {
    // This static field is used in javassist.ClassPoolTail to decide if opened
    // JarFiles should be cached. This is set to false, since the jdk jar files get
    // quite large, so this saves some memory at the cost of speed for the first
    // lookup per class. This will affect JavaParser JarTypeSolvers as well.
    ClassPool.cacheOpenedJarFile = false
    val classPool = new NonCachingClassPool()
    classPool
  }

  /** JavaParser replaces '$' in class names for nested classes with '.', while the names in the javassist class pool do
    * not. This means we need to keep a record of javaparser to classpool name for class pool lookups, e.g.
    * foo.bar.Baz.Qux -> foo.bar.Baz$Qux
    */
  private val javaParserToClassPoolNames = mutable.Map[String, String]()

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

  override def tryToSolveType(javaParserName: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    javaParserToClassPoolNames
      .get(javaParserName)
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

  private def addPathToClassPool(archivePath: String): Try[ClassPath] = {
    if (archivePath.isJarPath) {
      Try(classPool.appendClassPath(archivePath))
    } else if (archivePath.isJmodPath) {
      val classPath = new JmodClassPath(archivePath)
      Try(classPool.appendClassPath(classPath))
    } else {
      Failure(new IllegalArgumentException("$archivePath is not a path to a jar/jmod"))
    }
  }

  def withJars(archivePaths: Seq[String]): JdkJarTypeSolver = {
    addArchives(archivePaths)
    this
  }

  def addArchives(archivePaths: Seq[String]): Unit = {
    archivePaths.foreach { archivePath =>
      addPathToClassPool(archivePath) match {
        case Success(_) => registerKnownClassesForJar(archivePath)

        case Failure(e) =>
          logger.warn(s"Could not load jar at path $archivePath", e.getMessage())
      }
    }
  }

  private def registerJarEntry(jarEntry: JarEntry): Unit = {
    val entryName = jarEntry.getName()

    if (!jarEntry.isDirectory && entryName.endsWith(ClassExtension)) {
      val javaParserName = convertEntryPathToJavaParserName(entryName)
      val classPoolName  = convertEntryPathToClassPoolName(entryName)

      // Avoid keeping 2 identical copies of the name.
      if (javaParserName == classPoolName) {
        javaParserToClassPoolNames.put(javaParserName, javaParserName)
      } else {
        javaParserToClassPoolNames.put(javaParserName, classPoolName)
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
  val JarExtension: String    = ".jar"
  val JmodExtension: String   = ".jmod"

  extension (path: String) {
    def isJarPath: Boolean  = path.endsWith(JarExtension)
    def isJmodPath: Boolean = path.endsWith(JmodExtension)
  }

  def fromJdkPath(jdkPath: String): JdkJarTypeSolver = {
    val jarPaths = SourceFiles.determine(jdkPath, Set(JarExtension, JmodExtension))
    if (jarPaths.isEmpty) {
      throw new IllegalArgumentException(s"No .jar or .jmod files found at JDK path ${jdkPath}")
    }
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
