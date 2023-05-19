package io.joern.php2cpg.utils

import io.joern.php2cpg.utils.PathFilter.correctPath
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
    s"${startOrSep}${correctPath(dirName)}${anyDirSuffix}".r
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

  def correctPath(filename: String): String = {
    val nameParts = filename.split("/")
    Paths.get(nameParts.head, nameParts.tail: _*).toString
  }

  def apply(excludes: Option[List[String]]) = {
    new PathFilter(excludes)
  }
}
