package io.joern.javasrc2cpg.typesolvers

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
class JrtRuntimeImageClassPath(javaHome: Path) extends ClassPath {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val jrtFileSystem = JrtRuntimeImageClassPath.openJrtFileSystem(javaHome)

  private val (classNameToPath: Map[String, Path], moduleExports: Map[String, List[String]]) = buildIndexAndExports()

  val knownClassNames: Set[String] = classNameToPath.keySet

  /** Module name to exported package names (for JarTypeSolver.tryToSolveTypeInModule). */
  val moduleExportsMap: Map[String, List[String]] = moduleExports

  private def buildIndexAndExports(): (Map[String, Path], Map[String, List[String]]) = {
    val modulesRoot = jrtFileSystem.getPath("/modules")
    if (!Files.exists(modulesRoot)) {
      logger.warn(s"jrt: file system has no /modules root for java.home=$javaHome")
      (Map.empty, Map.empty)
    } else {
      val exportsBuilder = Map.newBuilder[String, List[String]]
      Using.resource(Files.list(modulesRoot)) { stream =>
        stream.iterator().asScala.foreach { moduleDir =>
          if (Files.isDirectory(moduleDir)) {
            val moduleInfo = moduleDir.resolve("module-info.class")
            if (Files.isRegularFile(moduleInfo)) {
              Try(Using.resource(Files.newInputStream(moduleInfo)) { in =>
                val (moduleName, exportPkgs) = JrtRuntimeImageClassPath.readModuleExports(in)
                exportsBuilder += moduleName -> exportPkgs
              }) match {
                case Failure(e) => logger.debug(s"Could not read module descriptor at $moduleInfo", e)
                case Success(_) =>
              }
            }
          }
        }
      }
      val exports = exportsBuilder.result()

      val indexMutable   = mutable.Map.empty[String, Path]
      var duplicateCount = 0
      Using.resource(Files.walk(modulesRoot)) { stream =>
        stream.iterator().asScala.foreach { path =>
          if (Files.isRegularFile(path) && path.getFileName.toString.endsWith(".class")) {
            if (path.getFileName.toString != "module-info.class") {
              Try(
                Using.resource(Files.newInputStream(path))(BytecodeIndexedClassPath.readClassNameFrom)
              ).toOption match {
                case Some(className) =>
                  if (indexMutable.contains(className)) {
                    duplicateCount += 1
                  }
                  indexMutable(className) = path
                case None =>
                  logger.debug(s"Could not read class name from $path")
              }
            }
          }
        }
      }
      if (duplicateCount > 0) {
        logger.debug(s"JRT index: $duplicateCount duplicate internal class names (last path wins)")
      }

      (indexMutable.toMap, exports)
    }
  }

  override def find(classname: String): URL = {
    classNameToPath
      .get(classname)
      .flatMap(p => Try(p.toUri.toURL).toOption)
      .orNull
  }

  override def openClassfile(classname: String): InputStream = {
    classNameToPath.get(classname).map(p => Files.newInputStream(p)).orNull
  }
}

object JrtRuntimeImageClassPath {

  /** Result of a single filesystem walk: the jrt java.home directory and the exact lib/modules jimage path. */
  final case class RuntimeImageLayout(javaHome: Path, modulesImageFile: Path)

  /** Parse a `module-info.class` stream and return the module name with its exported package names. */
  def readModuleExports(inputStream: InputStream): (String, List[String]) = {
    val descriptor = ModuleDescriptor.read(inputStream)
    (descriptor.name(), descriptor.exports().asScala.map(_.source()).toList)
  }

  /** Max directory depth when searching under findRuntimeImage (avoids scanning huge trees). */
  val DefaultRuntimeImageSearchMaxDepth: Int = 48

  /** Walk searchRoot once; return java.home and the modules file path used for jrt (no second tree walk). */
  def findRuntimeImage(
    searchRoot: Path,
    maxDepth: Int = DefaultRuntimeImageSearchMaxDepth
  ): Option[RuntimeImageLayout] = {
    if (!Files.exists(searchRoot)) {
      None
    } else {
      val root       = searchRoot.toAbsolutePath.normalize()
      val candidates = mutable.ArrayBuffer.empty[RuntimeImageLayout]
      // Two-arg walk does not follow symbolic links (consistent with jar discovery; avoids symlink loops).
      Using.resource(Files.walk(root, maxDepth)) { stream =>
        stream.iterator().asScala.foreach { p =>
          if (Files.isRegularFile(p) && !Files.isSymbolicLink(p) && p.getFileName.toString == "modules") {
            val lib = p.getParent
            if (
              lib != null && lib.getFileName != null && lib.getFileName.toString == "lib" && !Files.isSymbolicLink(lib)
            ) {
              val javaHome = lib.getParent
              if (javaHome != null) {
                candidates += RuntimeImageLayout(javaHome, p)
              }
            }
          }
        }
      }
      if (candidates.isEmpty) None
      else {
        // Prefer the shallowest match (e.g. jdkRoot/lib/modules over jdkRoot/extra/nested/lib/modules).
        Some(candidates.minBy { layout =>
          val rel = root.relativize(layout.javaHome)
          (rel.getNameCount, rel.toString)
        })
      }
    }
  }

  /** Directory layout root for a JDK / jlink image (java.home); see findRuntimeImage for the modules file path. */
  def findRuntimeImageRoot(searchRoot: Path, maxDepth: Int = DefaultRuntimeImageSearchMaxDepth): Option[Path] =
    findRuntimeImage(searchRoot, maxDepth).map(_.javaHome)

  private def openJrtFileSystem(javaHome: Path) = {
    val env = new java.util.HashMap[String, String]()
    env.put("java.home", javaHome.toAbsolutePath.normalize().toString)
    FileSystems.newFileSystem(URI.create("jrt:/"), env)
  }
}
