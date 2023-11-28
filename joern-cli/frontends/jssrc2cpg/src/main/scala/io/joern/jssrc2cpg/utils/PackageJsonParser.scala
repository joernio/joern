package io.joern.jssrc2cpg.utils

import java.nio.file.{Path, Paths}
import org.slf4j.LoggerFactory
import com.fasterxml.jackson.databind.ObjectMapper
import io.shiftleft.utils.IOUtils
import org.apache.commons.lang.StringUtils

import scala.collection.concurrent.TrieMap
import scala.util.Try
import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success

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
          val content      = IOUtils.readEntireFile(lockDepsPath)
          val objectMapper = new ObjectMapper
          val packageJson  = objectMapper.readTree(content)

          var depToVersion = Map.empty[String, String]
          val dependencyIt = Option(packageJson.get("dependencies"))
            .map(_.fields().asScala)
            .getOrElse(Iterator.empty)
          dependencyIt.foreach { entry =>
            val depName     = entry.getKey
            val versionNode = entry.getValue.get("version")
            if (versionNode != null) {
              depToVersion = depToVersion.updated(depName, versionNode.asText())
            }
          }
          depToVersion
        }.toOption

        // lazy val because we only evaluate this in case no package lock file is available.
        lazy val deps = Try {
          val content      = IOUtils.readEntireFile(depsPath)
          val objectMapper = new ObjectMapper
          val packageJson  = objectMapper.readTree(content)

          var depToVersion = Map.empty[String, String]
          ProjectDependencies
            .foreach { dependency =>
              val dependencyIt = Option(packageJson.get(dependency))
                .map(_.fields().asScala)
                .getOrElse(Iterator.empty)
              dependencyIt.foreach { entry =>
                depToVersion = depToVersion.updated(entry.getKey, entry.getValue.asText())
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
