package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.{TypeSolver, UnsolvedSymbolException}
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.model.SymbolReference
import com.github.javaparser.symbolsolver.javassistmodel.JavassistFactory
import io.joern.javasrc2cpg.typesolvers.JarTypeSolver.*
import io.joern.x2cpg.SourceFiles
import javassist.CtClass
import org.slf4j.LoggerFactory

import java.io.{File, IOException}
import java.lang.module.ModuleDescriptor
import java.nio.file.{Files, Path}
import java.util.jar.JarFile
import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

class JarTypeSolver(
  classPool: NonCachingClassPool,
  knownPackagePrefixes: Set[String],
  exportedPackages: Map[String, List[String]]
) extends TypeSolver {

  private var parent: Option[TypeSolver] = None

  private type RefType = ResolvedReferenceTypeDeclaration

  override def getParent(): TypeSolver = parent.get

  override def setParent(parent: TypeSolver): Unit = {
    this.parent match {
      case None =>
        this.parent = Some(parent)

      case Some(_) =>
        throw new RuntimeException("JarTypeSolver parent may only be set once")
    }
  }

  override def tryToSolveType(javaParserName: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    val packagePrefix = packagePrefixForJavaParserName(javaParserName)
    if (knownPackagePrefixes.contains(packagePrefix)) {
      lookupType(javaParserName)
    } else {
      SymbolReference.unsolved()
    }
  }

  private def lookupType(javaParserName: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    possibleStandardNamesForJavaParser(javaParserName).iterator
      .map(name => Try(classPool.get(name)))
      .collectFirst { case Success(ctClass) =>
        val refType = ctClassToRefType(ctClass)
        refTypeToSymbolReference(refType)
      }
      .getOrElse(SymbolReference.unsolved())
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

  override def tryToSolveTypeInModule(qualifiedModuleName: String, simpleTypeName: String): SymbolReference[RefType] = {
    exportedPackages
      .get(qualifiedModuleName)
      .iterator
      .flatten
      .map { exportedPackage =>
        val fullName = s"$exportedPackage.$simpleTypeName"
        tryToSolveType(fullName)
      }
      .collectFirst { case typ if typ.isSolved => typ }
      .getOrElse(SymbolReference.unsolved())
  }
}

class JarTypeSolverBuilder(enableVerboseTypeLogging: Boolean) {

  private val logger                                    = LoggerFactory.getLogger(this.getClass)
  private val classPool                                 = new NonCachingClassPool()
  private val knownPackagePrefixes: mutable.Set[String] = mutable.Set.empty
  // A map of module name -> exported package names
  private val exportedPackages: mutable.Map[String, List[String]] = mutable.Map.empty

  def build: JarTypeSolver = {
    new JarTypeSolver(classPool, knownPackagePrefixes.toSet, exportedPackages.toMap)
  }

  private def addPathToClassPool(archivePath: String): Try[BytecodeIndexedClassPath] = {
    if (archivePath.isJarPath || archivePath.isJmodPath) {
      Try {
        val classPath = new BytecodeIndexedClassPath(archivePath)
        classPool.appendClassPath(classPath)
        classPath
      }
    } else {
      Failure(new IllegalArgumentException(s"$archivePath is not a path to a jar/jmod"))
    }
  }

  def withJars(archivePaths: Seq[String]): JarTypeSolverBuilder = {
    addArchives(archivePaths)
    this
  }

  private def addArchives(archivePaths: Seq[String]): Unit = {
    archivePaths.foreach { archivePath =>
      addPathToClassPool(archivePath) match {
        case Success(classPath) =>
          registerPackagesFromClassNames(archivePath, classPath.knownClassNames)

        case Failure(e) =>
          logger.warn(s"Could not load jar at path $archivePath", e.getMessage())
      }
    }
  }

  private def registerExportedPackages(jarFile: JarFile): Unit = {
    jarFile.entries().asScala.filter(_.getName.endsWith("module-info.class")).foreach { moduleInfoEntry =>
      Using(jarFile.getInputStream(moduleInfoEntry)) { inputStream =>
        // TODO: Handle qualified exports if the JavaParser type solver adds support for that
        val descriptor = ModuleDescriptor.read(inputStream)
        val moduleName = descriptor.name()
        val exports    = descriptor.exports().asScala.map(_.source()).toList

        exportedPackages.put(moduleName, exports)
      }
    }
  }

  /** Register package prefixes from actual class names read from bytecode. Used for JAR files where the entry path may
    * not match the declared package.
    */
  private def registerPackagesFromClassNames(archivePath: String, classNames: Set[String]): Unit = {
    if (enableVerboseTypeLogging) {
      logger.debug(s"Adding jar to JarTypeSolver: $archivePath")
      classNames.foreach(name => logger.debug(s" - $name"))
    }
    val newPrefixes = classNames.map(packagePrefixForJavaParserName)
    knownPackagePrefixes ++= newPrefixes
    registerExportedPackagesForArchive(archivePath)
  }

  private def registerExportedPackagesForArchive(archivePath: String): Unit = {
    try {
      Using(new JarFile(archivePath)) { jarFile =>
        registerExportedPackages(jarFile)
      }
    } catch {
      case ioException: IOException =>
        logger.warn(s"Could not register exported packages for archive at $archivePath", ioException.getMessage())
    }
  }
}

object JarTypeSolver {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val JarExtension: String                                     = ".jar"
  val JmodExtension: String                                    = ".jmod"
  private val cache: mutable.Map[String, JarTypeSolverBuilder] = mutable.Map.empty

  extension (path: String) {
    def isJarPath: Boolean  = path.endsWith(JarExtension)
    def isJmodPath: Boolean = path.endsWith(JmodExtension)
  }

  private def determineJarPaths(jdkPath: String): List[String] = {
    // not following symlinks, because some setups might have a loop, e.g. AWS's Corretto
    // see https://github.com/joernio/joern/pull/3871
    val jarPaths = SourceFiles.determine(jdkPath, Set(JarExtension, JmodExtension))(Seq.empty)
    val jarsFound = jarPaths.mkString("\n - ")
    logger.warn(s"Jars found:\n - $jarsFound")
    Try(new File(jdkPath)).foreach { jdkPathFile =>
      logger.warn(s"jdkPathFileExists: ${jdkPathFile.exists()}")
      logger.warn(s"jdkPathIsDirectory: ${jdkPathFile.isDirectory()}")
      val foundFiles = Files.walk(jdkPathFile.toPath.toAbsolutePath).collect(Collectors.toList).asScala.map(_.toAbsolutePath).mkString("\n - ")
      logger.warn(s"jdkPathSubfiles:\n - ${foundFiles}")
    }

    if (jarPaths.isEmpty) {
      throw new IllegalArgumentException(s"No .jar or .jmod files found at JDK path ${jdkPath}")
    }
    jarPaths
  }

  def fromJdkPath(
    jdkPath: String,
    useCache: Boolean = false,
    enableVerboseTypeLogging: Boolean = false
  ): JarTypeSolver = {
    def createBuilder = new JarTypeSolverBuilder(enableVerboseTypeLogging).withJars(determineJarPaths(jdkPath))
    if (useCache) {
      cache.getOrElseUpdate(jdkPath, createBuilder).build
    } else {
      createBuilder.build
    }
  }

  /** Convert JavaParser class name foo.bar.qux.Baz to package prefix foo.bar Only use first 2 parts since this is
    * sufficient to deterimine whether a class has been registered in most cases and, if not, the failure is just a slow
    * lookup.
    */
  def packagePrefixForJavaParserName(className: String): String = {
    className.split("\\.").take(2).mkString(".")
  }

  /** JavaParser replaces the `$` in nested class names with a `.`. This means that we cannot know what the standard
    * type full name is for JavaParser names with multiple parts, so this method returns all possibilities, for example
    * for a.b.Foo.Bar, it will return:
    *   - a.b.Foo.Bar
    *   - a.b.Foo$Bar
    *   - a.b$Foo$Bar
    *   - a$b$Foo$Bar
    */
  def possibleStandardNamesForJavaParser(javaParserName: String): List[String] = {
    val nameParts = javaParserName.split('.')
    nameParts.indices.reverse.map { packageLength =>
      val packageName = nameParts.take(packageLength).mkString(".")
      val className   = nameParts.drop(packageLength).mkString("$")

      val packagePrefix = if (packageLength > 0) s"$packageName." else ""

      s"$packagePrefix$className"
    }.toList
  }
}
