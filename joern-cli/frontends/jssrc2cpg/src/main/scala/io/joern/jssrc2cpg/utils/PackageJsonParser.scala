package io.joern.jssrc2cpg.utils

import io.shiftleft.utils.IOUtils
import org.apache.commons.lang3.StringUtils
import org.slf4j.LoggerFactory
import upickle.default.read

import java.nio.file.{Path, Paths}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

object PackageJsonParser {
  private val logger = LoggerFactory.getLogger(PackageJsonParser.getClass)

  val PackageJsonFilename     = "package.json"
  val PackageJsonLockFilename = "package-lock.json"

  private val ProjectDependencies =
    Seq("dependencies", "devDependencies", "peerDependencies", "optionalDependencies")

  private val cachedDependencies: TrieMap[Path, Map[String, String]] = TrieMap.empty

  def isValidProjectPackageJson(packageJsonPath: Path): Boolean = {
    if (packageJsonPath.toString.endsWith(PackageJsonParser.PackageJsonFilename)) {
      val isNotEmpty = Try(IOUtils.readLinesInFile(packageJsonPath)) match {
        case Success(content) =>
          content.forall(l => StringUtils.isNotBlank(StringUtils.normalizeSpace(l)))
        case Failure(_) => false
      }
      isNotEmpty && dependencies(packageJsonPath).nonEmpty
    } else {
      false
    }
  }

  def dependencies(packageJsonPath: Path): Map[String, String] =
    cachedDependencies.getOrElseUpdate(
      packageJsonPath, {
        val depsPath     = packageJsonPath
        val lockDepsPath = packageJsonPath.resolveSibling(Paths.get(PackageJsonLockFilename))

        val lockDeps = Try {
          val content     = IOUtils.readEntireFile(lockDepsPath)
          val packageJson = read[ujson.Obj](content)

          var depToVersion = Map.empty[String, String]
          val dependencyIt = packageJson.value.get("dependencies").map(_.obj).getOrElse(Map.empty[String, ujson.Value])
          dependencyIt.foreach {
            case (depName, value @ ujson.Str(version)) =>
              depToVersion = depToVersion.updated(depName, version)
            case (depName, value @ ujson.Obj(obj)) =>
              obj.get("version").foreach { version =>
                depToVersion = depToVersion.updated(depName, version.str)
              }
            case (depName, value) =>
              logger.warn(s"Unexpected version structure for dependency $depName: ${value.getClass}")
          }
          depToVersion
        }.toOption

        // lazy val because we only evaluate this in case no package lock file is available.
        lazy val deps = Try {
          val content     = IOUtils.readEntireFile(depsPath)
          val packageJson = read[ujson.Obj](content)

          var depToVersion = Map.empty[String, String]
          ProjectDependencies
            .foreach { dependency =>
              val dependencyIt = packageJson.value.get(dependency).map(_.obj).getOrElse(Map.empty[String, ujson.Value])
              dependencyIt.foreach { case (key, value) =>
                depToVersion = depToVersion.updated(key, value.str)
              }
            }
          depToVersion
        }.toOption

        if (lockDeps.isDefined && lockDeps.get.nonEmpty) {
          logger.debug(s"Loaded dependencies from '$lockDepsPath'.")
          lockDeps.get
        } else {
          if (deps.isDefined && deps.get.nonEmpty) {
            logger.debug(s"Loaded dependencies from '$depsPath'.")
            deps.get
          } else {
            logger.debug(
              s"No project dependencies found in $PackageJsonFilename or $PackageJsonLockFilename at '${depsPath.getParent}'."
            )
            Map.empty
          }
        }
      }
    )

}
