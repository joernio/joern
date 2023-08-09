package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
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
import io.shiftleft.semanticcpg.language.singleToEvalTypeAccessorsParameterOut
import com.github.javaparser.resolution.TypeSolver
import com.github.javaparser.resolution.model.SymbolReference

class JdkJarTypeSolver private (jdkPath: String) extends TypeSolver {

  private val logger = LoggerFactory.getLogger(this.getClass())

  private var parent: Option[TypeSolver] = None
  private val classPool                  = new NonCachingClassPool()

  private val knownPackagePrefixes: mutable.Set[String] = mutable.Set.empty

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
    val packagePrefix = packagePrefixForJavaParserName(javaParserName)
    if (knownPackagePrefixes.contains(packagePrefix)) {
      lookupType(javaParserName)
    } else {
      SymbolReference.unsolved(classOf[RefType])
    }
  }

  private def lookupType(javaParserName: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    val name = convertJavaParserNameToStandard(javaParserName)
    Try(classPool.get(name)) match {
      case Success(ctClass) =>
        val refType = ctClassToRefType(ctClass)
        refTypeToSymbolReference(refType)

      case Failure(e) =>
        SymbolReference.unsolved(classOf[RefType])
    }
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
        case Success(_) => registerPackagesForJar(archivePath)

        case Failure(e) =>
          logger.warn(s"Could not load jar at path $archivePath", e.getMessage())
      }
    }
  }

  private def registerPackagesForJar(archivePath: String): Unit = {
    val entryNameConverter = if (archivePath.isJarPath) packagePrefixForJarEntry else packagePrefixForJmodEntry
    try {
      Using(new JarFile(archivePath)) { jarFile =>
        knownPackagePrefixes ++=
          jarFile
            .entries()
            .asIterator()
            .asScala
            .filter(entry => !entry.isDirectory() && entry.getName().endsWith(ClassExtension))
            .map(entry => entryNameConverter(entry.getName()))
      }
    } catch {
      case ioException: IOException =>
        logger.warn(s"Could register classes for archive at $archivePath", ioException.getMessage())
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

  /** Convert JavaParser class name foo.bar.qux.Baz to package prefix foo.bar Only use first 2 parts since this is
    * sufficient to deterimine whether a class has been registered in most cases and, if not, the failure is just a slow
    * lookup.
    */
  def packagePrefixForJavaParserName(className: String): String = {
    className.split("\\.").take(2).mkString(".")
  }

  /** Convert Jar entry name foo/bar/qux/Baz.class to package prefix foo.bar Only use first 2 parts since this is
    * sufficient to deterimine whether a class has been registered in most cases and, if not, the failure is just a slow
    * lookup.
    */
  def packagePrefixForJarEntry(entryName: String): String = {
    entryName.split("/").take(2).mkString(".")
  }

  /** Convert jmod entry name classes/foo/bar/qux/Baz.class to package prefix foo.bar Only use first 2 parts since this
    * is sufficient to deterimine whether a class has been registered in most cases and, if not, the failure is just a
    * slow lookup.
    */
  def packagePrefixForJmodEntry(entryName: String): String = {
    packagePrefixForJarEntry(entryName.stripPrefix(JmodClassPrefix))
  }

  /** A name is assumed to contain at least one subclass (e.g. ...Foo$Bar) if the last name part starts with a digit, or
    * if the last 2 name parts start with capital letters. This heuristic is based on the class name format in the JDK
    * jars, where names with subclasses have one of the forms:
    *   - java.lang.ClassLoader$2
    *   - java.lang.ClassLoader$NativeLibrary
    *   - java.lang.ClassLoader$NativeLibrary$Unloader
    */
  private def namePartsContainSubclass(nameParts: Array[String]): Boolean = {
    nameParts.takeRight(2) match {
      case Array() => false

      case Array(singlePart) => false

      case Array(secondLast, last) =>
        last.head.isDigit || (secondLast.head.isUpper && last.head.isUpper)
    }
  }

  /** JavaParser replaces the `$` in nested class names with a `.`. This method converts the JavaParser names to the
    * standard format by replacing the `.` between name parts that start with a capital letter or a digit with a `$`
    * since the jdk classes follow the standard practice of capitalising the first letter in class names but not package
    * names.
    */
  def convertJavaParserNameToStandard(className: String): String = {
    className.split(".") match {
      case nameParts if namePartsContainSubclass(nameParts) =>
        val (packagePrefix, classNames) = nameParts.partition(_.head.isLower)
        s"${packagePrefix.mkString(".")}.${classNames.mkString("$")}"

      case _ => className
    }

  }
}
