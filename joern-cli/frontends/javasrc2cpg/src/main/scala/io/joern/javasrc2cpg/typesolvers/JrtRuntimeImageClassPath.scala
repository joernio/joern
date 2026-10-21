package io.joern.javasrc2cpg.typesolvers

import io.joern.javasrc2cpg.typesolvers.JrtRuntimeImageClassPath.logger
import javassist.ClassPath
import org.slf4j.LoggerFactory

import java.io.InputStream
import java.lang.module.ModuleDescriptor
import java.net.{URI, URL}
import java.nio.file.{FileSystems, Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

/** ClassPath over the JDK modular runtime image (JEP 220, lib/modules jimage), exposed via the jrt file system. Used
  * when the configured JDK path has no jar/jmod archives to index (e.g. minimal jlink images). When a full JDK is
  * present, BytecodeIndexedClassPath on jmods and other archives is preferred instead (see JarTypeSolver.fromPath).
  *
  * The FileSystem is kept open for the lifetime of this instance (same lifecycle as BytecodeIndexedClassPath holding a
  * JarFile).
  */
class JrtRuntimeImageClassPath(javaHome: Path) extends ClassPath with AutoCloseable {

  // The jrtFileSystem needs to stay open for the entire javasrc2cpg lifetime since all paths indexed
  // and used here are paths within the jrtfs, so it is kept as a private member here.
  private val jrtFileSystem = JrtRuntimeImageClassPath.openJrtFileSystem(javaHome)

  private val (classNameToPath: Map[String, Path], moduleExports: Map[String, List[String]]) = {
    val modulesRoot = jrtFileSystem.getPath("/modules")
    if (!Files.exists(modulesRoot)) {
      logger.warn(s"jrt: file system has no /modules root for java.home=$javaHome")
      (Map.empty[String, Path], Map.empty[String, List[String]])
    } else {
      buildExportsAndIndex(modulesRoot)
    }
  }

  val knownClassNames: Set[String] = classNameToPath.keySet

  /** Module name to exported package names (for JarTypeSolver.tryToSolveTypeInModule). */
  val moduleExportsMap: Map[String, List[String]] = moduleExports

  private def buildExportsAndIndex(modulesRoot: Path): (Map[String, Path], Map[String, List[String]]) = {
    val exportsBuilder = Map.newBuilder[String, List[String]]
    val indexMutable   = mutable.Map.empty[String, Path]
    var duplicateCount = 0

    Using.resource(Files.walk(modulesRoot)) { recursiveFiles =>
      recursiveFiles.iterator().asScala.foreach { path =>
        if (Files.isRegularFile(path) && path.getFileName.toString.endsWith(".class")) {
          Try(Using.resource(Files.newInputStream(path)) { recursiveFiles =>
            if (path.getFileName.toString == "module-info.class") {
              handleModuleInfo(recursiveFiles, exportsBuilder)
            } else {
              if (handleClassFile(recursiveFiles, path, indexMutable)) {
                duplicateCount += 1
              }
            }
          }).recover { case e => logger.debug(s"Could not read class files at $path", e) }
        }
      }
    }

    if (duplicateCount > 0) {
      logger.debug(s"JRT index: $duplicateCount duplicate class names across modules (first module wins)")
    }

    (indexMutable.toMap, exportsBuilder.result())
  }

  private def handleModuleInfo(
    in: InputStream,
    exportsBuilder: mutable.Builder[(String, List[String]), Map[String, List[String]]]
  ): Unit = {
    val (moduleName, exportPkgs) = JrtRuntimeImageClassPath.readModuleExports(in)
    exportsBuilder += moduleName -> exportPkgs
  }

  /** Returns true if the class name was a duplicate. */
  private def handleClassFile(in: InputStream, path: Path, indexMutable: mutable.Map[String, Path]): Boolean = {
    val className = BytecodeIndexedClassPath.readClassNameFrom(in)
    if (!indexMutable.contains(className)) {
      indexMutable(className) = path
      false
    } else {
      true
    }
  }

  override def find(classname: String): URL = {
    classNameToPath
      .get(classname)
      .flatMap(path => Try(path.toUri.toURL).toOption)
      .orNull
  }

  override def openClassfile(classname: String): InputStream = {
    classNameToPath.get(classname).map(path => Files.newInputStream(path)).orNull
  }

  override def close(): Unit = jrtFileSystem.close()
}

object JrtRuntimeImageClassPath {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Result of a single filesystem walk: the jrt java.home directory and the exact lib/modules jimage path. */
  final case class RuntimeImageLayout(javaHome: Path, modulesImageFile: Path)

  /** Parse a `module-info.class` stream and return the module name with its exported package names. */
  def readModuleExports(inputStream: InputStream): (String, List[String]) = {
    val descriptor = ModuleDescriptor.read(inputStream)
    (descriptor.name(), descriptor.exports().asScala.map(_.source()).toList)
  }

  /** Max directory depth (in terms of file depth from the search root) when searching under findRuntimeImage. Since
    * `lib/modules` sits 2 levels below a java.home, a maxDepth of 6 allows java.home directories up to 4 levels deep.
    */
  val DefaultRuntimeImageRootSearchMaxDepth: Int = 10

  /** Find the shallowest `<java.home>/lib/modules` layout under searchRoot. maxDepth limits how deep the `modules` file
    * itself can be. Does not follow symbolic links (avoids symlink loops, consistent with jar discovery).
    */
  def findRuntimeImage(searchRoot: Path, maxDepth: Int = DefaultRuntimeImageRootSearchMaxDepth): Option[Path] = {
    if (!Files.exists(searchRoot)) {
      None
    } else {
      val root = searchRoot.toAbsolutePath.normalize()
      Using.resource(Files.walk(root, maxDepth)) { recursiveFiles =>
        recursiveFiles
          .iterator()
          .asScala
          .filter(p => Files.isRegularFile(p) && !Files.isSymbolicLink(p) && p.getFileName.toString == "modules")
          .flatMap { path =>
            for {
              lib <- Option(path.getParent)
              if (lib.getFileName != null && lib.getFileName.toString == "lib" && !Files.isSymbolicLink(lib))
              javaHome <- Option(lib.getParent)
            } yield javaHome
          }
          .toList
          .match {
            case Nil =>
              logger.debug("No modules runtime image found")
              None

            case singleResult :: Nil =>
              Some(singleResult)

            case firstResult :: _ =>
              logger.warn(s"Found multiple modules runtime images. Using the first found at ${firstResult}")
              Some(firstResult)

          }
      }
    }
  }

  private def openJrtFileSystem(javaHome: Path) = {
    val env = new java.util.HashMap[String, String]()
    env.put("java.home", javaHome.toAbsolutePath.normalize().toString)
    FileSystems.newFileSystem(URI.create("jrt:/"), env)
  }
}
