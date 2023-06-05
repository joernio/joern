package io.joern.php2cpg.utils

import io.joern.php2cpg.utils.PathFilter.standardisePath
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.Paths
import scala.util.matching.Regex

class PathFilter(excludeOverrides: Option[List[String]]) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private val sep          = if (File.separator == "/") File.separator else "\\\\"
  private val startOrSep   = s"^([^$sep]+$sep)*"
  private val anyDirSuffix = s"$sep.*"

  def dirExclude(dirName: String): Regex = {
    val path = escapeSeparators(standardisePath(dirName))
    s"${startOrSep}$path${anyDirSuffix}".r
  }

  private def escapeSeparators(filename: String): String = {
    if (File.separator == "/")
      filename
    else
      filename.replaceAll(raw"\\", raw"\\\\")
  }

  private val excludeRegexes = {
    excludeOverrides.getOrElse(PathFilter.DefaultExcludes).map(dirExclude)
  }

  def excluded(filename: String): Boolean = {
    excludeRegexes.exists(_.matches(filename))
  }
}

object PathFilter {
  val DefaultExcludes: List[String] = List(
    "tests",
    // For composer projects
    "vendor",
    ".git"
  )

  def standardisePath(filename: String): String = {
    // Synthetic filename, so don't correct
    if (filename.contains("<"))
      filename
    else
      Paths.get(filename).toString
  }

  def apply(excludes: Option[List[String]]): PathFilter = {
    new PathFilter(excludes)
  }
}
