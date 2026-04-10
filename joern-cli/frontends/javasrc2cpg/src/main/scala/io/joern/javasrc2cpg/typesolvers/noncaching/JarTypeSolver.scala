package io.joern.javasrc2cpg.typesolvers

import com.github.javaparser.resolution.{TypeSolver, UnsolvedSymbolException}
import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.resolution.model.SymbolReference
import com.github.javaparser.symbolsolver.javassistmodel.JavassistFactory
import io.joern.javasrc2cpg.typesolvers.JarTypeSolver.*
import io.joern.x2cpg.SourceFiles
import javassist.CtClass
import org.slf4j.LoggerFactory

import java.io.IOException
import java.nio.file.{Path, Paths}
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

class JarTypeSolver(
  classPool: NonCachingClassPool,
  knownPackagePrefixes: Set[String],
  exportedPackages: Map[String, List[String]],
  private val closeables: Seq[AutoCloseable] = Seq.empty
) extends TypeSolver
    with AutoCloseable {

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

  override def close(): Unit = closeables.foreach(c => Try(c.close()))

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
  private val ownedCloseables: mutable.ArrayBuffer[AutoCloseable] = mutable.ArrayBuffer.empty

  /** Build a solver that owns its classpath resources and closes them when [[JarTypeSolver.close]] is called. */
  def build: JarTypeSolver =
    new JarTypeSolver(classPool, knownPackagePrefixes.toSet, exportedPackages.toMap, ownedCloseables.toSeq)

  /** Build a solver that does NOT own the classpath resources (used for cached builders whose resources must outlive
    * any single solver instance).
    */
  private[typesolvers] def buildShared: JarTypeSolver =
    new JarTypeSolver(classPool, knownPackagePrefixes.toSet, exportedPackages.toMap)

  private def addPathToClassPool(archivePath: String): Try[BytecodeIndexedClassPath] = {
    if (archivePath.isJarPath || archivePath.isJmodPath) {
      Try {
        val classPath = new BytecodeIndexedClassPath(archivePath)
        classPool.appendClassPath(classPath)
        ownedCloseables += classPath
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
          registerPackagesFromClassNames(
            archivePath,
            classPath.knownClassNames,
            jarExportArchivePath = Some(archivePath)
          )

        case Failure(e) =>
          logger.warn(s"Could not load jar at path $archivePath", e.getMessage())
      }
    }
  }

  /** Load the `jrt:` runtime image (`lib/modules`) for a minimal JDK/jlink layout. */
  private[typesolvers] def addRuntimeImage(jdkRoot: Path): Try[Unit] = {
    Try {
      val jrt = new JrtRuntimeImageClassPath(jdkRoot)
      classPool.appendClassPath(jrt)
      ownedCloseables += jrt
      registerPackagesFromClassNames(jdkRoot.toString, jrt.knownClassNames, jarExportArchivePath = None)
      exportedPackages ++= jrt.moduleExportsMap
    }
  }

  private def registerExportedPackages(jarFile: JarFile): Unit = {
    jarFile.entries().asScala.filter(_.getName.endsWith("module-info.class")).foreach { moduleInfoEntry =>
      Using(jarFile.getInputStream(moduleInfoEntry)) { inputStream =>
        // TODO: Handle qualified exports if the JavaParser type solver adds support for that
        val (moduleName, exports) = JrtRuntimeImageClassPath.readModuleExports(inputStream)
        exportedPackages.put(moduleName, exports)
      }
    }
  }

  /** Register package prefixes from actual class names read from bytecode. Used for JAR files where the entry path may
    * not match the declared package. When `jarExportArchivePath` is set, also reads `module-info.class` from that JAR;
    * for the `jrt:` runtime image, exports are registered separately in [[addRuntimeImage]].
    */
  private def registerPackagesFromClassNames(
    sourceLabel: String,
    classNames: Set[String],
    jarExportArchivePath: Option[String]
  ): Unit = {
    if (enableVerboseTypeLogging) {
      logger.debug(s"Adding types to JarTypeSolver: $sourceLabel")
      classNames.foreach(name => logger.debug(s" - $name"))
    }
    val newPrefixes = classNames.map(packagePrefixForJavaParserName)
    knownPackagePrefixes ++= newPrefixes
    jarExportArchivePath.foreach(registerExportedPackagesForArchive)
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
  val JarExtension: String                                     = ".jar"
  val JmodExtension: String                                    = ".jmod"
  private val cache: mutable.Map[String, JarTypeSolverBuilder] = mutable.Map.empty
  private val logger                                           = LoggerFactory.getLogger(classOf[JarTypeSolver])

  extension (path: String) {
    def isJarPath: Boolean  = path.endsWith(JarExtension)
    def isJmodPath: Boolean = path.endsWith(JmodExtension)
  }

  /** All `.jar` / `.jmod` paths under `inputPath`, or empty if none (may still use `lib/modules` via
    * [[JrtRuntimeImageClassPath]]).
    */
  private def determineJarPathsAllowEmpty(inputPath: String): List[String] = {
    // not following symlinks, because some setups might have a loop, e.g. AWS's Corretto
    // see https://github.com/joernio/joern/pull/3871
    SourceFiles.determine(inputPath, Set(JarExtension, JmodExtension))(Seq.empty)
  }

  def fromPath(
    inputPath: String,
    useCache: Boolean = false,
    enableVerboseTypeLogging: Boolean = false
  ): JarTypeSolver = {
    def createBuilder: JarTypeSolverBuilder = {
      val jarPaths = determineJarPathsAllowEmpty(inputPath)
      val builder  = new JarTypeSolverBuilder(enableVerboseTypeLogging)
      if (jarPaths.nonEmpty) {
        logger.info(s"JDK type solver: using ${jarPaths.size} jar/jmod archive(s) under $inputPath")
        builder.withJars(jarPaths)
      } else {
        JrtRuntimeImageClassPath.findRuntimeImage(Paths.get(inputPath)) match {
          case Some(imageRootPath) =>
            logger.info(
              s"JDK type solver: using runtime image at $imageRootPath; search root: $inputPath)"
            )
            builder.addRuntimeImage(imageRootPath) match {
              case Success(_) => builder
              case Failure(exception) =>
                throw new IllegalArgumentException(
                  s"Could not load JDK runtime image (jrt:) for image root path=$imageRootPath",
                  exception
                )
            }
          case None =>
            throw new IllegalArgumentException(
              s"No .jar or .jmod files found under $inputPath, and no runtime image file at .../lib/modules beneath that path"
            )
        }
      }
    }
    if (useCache) {
      cache.getOrElseUpdate(inputPath, createBuilder).buildShared
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
