package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.{NewDependency}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.matching.Regex

// This pass takes information out of specific CONFIG_FILE nodes in order to add DEPENDENCY nodes to the graph.
// It supports:
//   - requirements.txt (with all common version specifiers: ==, >=, <=, ~=, !=, >, <)
//   - pyproject.toml ([project.dependencies] and [tool.poetry.dependencies])
//   - setup.cfg ([options] install_requires)
class DependenciesFromRequirementsTxtPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[DependenciesFromRequirementsTxtPass])

  // Matches a package name (with optional extras like [extra1,extra2]) followed by a version specifier.
  // Captures: group(1) = package name, group(2) = version specifier operator, group(3) = version string
  // Handles environment markers by stripping them (everything after ';')
  private val RequirementsLinePattern: Regex =
    """^\s*([\w][\w.\-]*)\s*(?:\[[\w,.\s\-]*\])?\s*(~=|==|!=|>=|<=|>|<)\s*([\w.*+!\-]+)\s*(?:;.*)?$""".r

  // Matches a package line without any version specifier (just a bare package name, possibly with extras)
  private val BarePackagePattern: Regex =
    """^\s*([\w][\w.\-]*)\s*(?:\[[\w,.\s\-]*\])?\s*(?:;.*)?$""".r

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    cpg.configFile.foreach { node =>
      if (node.name.endsWith("requirements.txt")) {
        parseRequirementsTxt(node.content, dstGraph)
      } else if (node.name.endsWith("pyproject.toml")) {
        parsePyprojectToml(node.content, dstGraph)
      } else if (node.name.endsWith("setup.cfg")) {
        parseSetupCfg(node.content, dstGraph)
      }
    }
  }

  /** Parse requirements.txt content and add dependency nodes. Handles:
    *   - Exact version pinning: package==1.0.0
    *   - Other version specifiers: package>=1.0, package~=1.0, package!=1.0, package>1.0, package<2.0, package<=2.0
    *   - Extras: package[extra]>=1.0
    *   - Environment markers: package>=1.0; python_version >= "3.8"
    *   - Comment lines (starting with #)
    *   - Include references (-r other.txt) are skipped
    *   - Blank lines are skipped
    */
  private def parseRequirementsTxt(content: String, dstGraph: DiffGraphBuilder): Unit = {
    val lines = content.split("\n")
    lines.foreach { rawLine =>
      val line = rawLine.trim
      // Skip comments, blank lines, -r/-c includes, and options starting with --
      if (line.nonEmpty && !line.startsWith("#") && !line.startsWith("-r") && !line.startsWith("-c") && !line
          .startsWith("--")) {
        line match {
          case RequirementsLinePattern(name, _, version) =>
            addDependency(dstGraph, name.trim, version.trim)
          case BarePackagePattern(name) =>
            addDependency(dstGraph, name.trim, "")
          case _ => // skip lines that don't match (e.g., URLs, editable installs)
        }
      }
    }
  }

  /** Parse pyproject.toml content and add dependency nodes from:
    *   - [project.dependencies] section (PEP 621 format)
    *   - [tool.poetry.dependencies] section (Poetry format)
    */
  private def parsePyprojectToml(content: String, dstGraph: DiffGraphBuilder): Unit = {
    parsePep621Dependencies(content, dstGraph)
    parsePoetryDependencies(content, dstGraph)
  }

  /** Parse PEP 621 style dependencies from [project] section:
    * {{{
    * [project]
    * dependencies = [
    *   "Flask>=2.0",
    *   "requests==2.28.0",
    * ]
    * }}}
    */
  private def parsePep621Dependencies(content: String, dstGraph: DiffGraphBuilder): Unit = {
    // Find the dependencies array in [project] section
    // We look for a line starting with "dependencies" followed by "=" and "[", then collect lines until "]"
    val lines            = content.split("\n")
    var inProjectSection = false
    var inDepsArray      = false
    var bracketDepth     = 0

    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[") && !trimmed.startsWith("[[")) {
        inProjectSection = trimmed == "[project]"
        if (!inProjectSection) inDepsArray = false
      } else if (inProjectSection && !inDepsArray && trimmed.startsWith("dependencies")) {
        // Check if this line starts the dependencies array
        val afterEquals = trimmed.dropWhile(_ != '=').drop(1).trim
        if (afterEquals.startsWith("[")) {
          inDepsArray = true
          bracketDepth = 1
          // There might be entries on this same line
          parseDepsFromTomlArrayLine(afterEquals.drop(1), dstGraph)
          if (afterEquals.contains("]")) {
            inDepsArray = false
            bracketDepth = 0
          }
        }
      } else if (inDepsArray) {
        if (trimmed.contains("]")) {
          // Parse any entries before the closing bracket
          parseDepsFromTomlArrayLine(trimmed.takeWhile(_ != ']'), dstGraph)
          inDepsArray = false
          bracketDepth = 0
        } else {
          parseDepsFromTomlArrayLine(trimmed, dstGraph)
        }
      }
    }
  }

  /** Parse a line from a TOML dependencies array, extracting quoted dependency strings. */
  private def parseDepsFromTomlArrayLine(line: String, dstGraph: DiffGraphBuilder): Unit = {
    // Extract quoted strings from the line (supports both single and double quotes)
    val QuotedStringPattern: Regex = """["']([^"']+)["']""".r
    for (m <- QuotedStringPattern.findAllMatchIn(line)) {
      val depString = m.group(1).trim
      depString match {
        case RequirementsLinePattern(name, _, version) =>
          addDependency(dstGraph, name.trim, version.trim)
        case BarePackagePattern(name) =>
          addDependency(dstGraph, name.trim, "")
        case _ => // skip
      }
    }
  }

  /** Parse Poetry-style dependencies from [tool.poetry.dependencies]:
    * {{{
    * [tool.poetry.dependencies]
    * python = "^3.8"
    * Flask = ">=2.0"
    * requests = "2.28.0"
    * numpy = {version = ">=1.20", optional = true}
    * }}}
    */
  private def parsePoetryDependencies(content: String, dstGraph: DiffGraphBuilder): Unit = {
    val lines                = content.split("\n")
    var inPoetryDepsSection  = false
    val PoetryDepPattern     = """^\s*([\w][\w.\-]*)\s*=\s*"([^"]*)".*$""".r
    val PoetryDepDictPattern = """^\s*([\w][\w.\-]*)\s*=\s*\{.*version\s*=\s*"([^"]*)".*\}.*$""".r
    val VersionExtract       = """^([~^>=<!]+)?\s*(.+)$""".r

    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[") && !trimmed.startsWith("[[")) {
        inPoetryDepsSection = trimmed == "[tool.poetry.dependencies]"
      } else if (inPoetryDepsSection && trimmed.nonEmpty && !trimmed.startsWith("#")) {
        val depMatch = trimmed match {
          case PoetryDepDictPattern(name, versionSpec) => Some((name, versionSpec))
          case PoetryDepPattern(name, versionSpec)     => Some((name, versionSpec))
          case _                                       => None
        }
        depMatch.foreach { case (name, versionSpec) =>
          // Skip the python version entry
          if (name != "python") {
            versionSpec match {
              case VersionExtract(_, version) =>
                addDependency(dstGraph, name.trim, version.trim)
              case _ =>
                addDependency(dstGraph, name.trim, versionSpec.trim)
            }
          }
        }
      }
    }
  }

  /** Parse setup.cfg content and add dependency nodes from [options] install_requires:
    * {{{
    * [options]
    * install_requires =
    *     Flask>=2.0
    *     requests==2.28.0
    *     numpy
    * }}}
    */
  private def parseSetupCfg(content: String, dstGraph: DiffGraphBuilder): Unit = {
    val lines            = content.split("\n")
    var inOptionsSection = false
    var inInstallReqs    = false

    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[")) {
        inOptionsSection = trimmed == "[options]"
        if (!inOptionsSection) inInstallReqs = false
      } else if (inOptionsSection && !inInstallReqs && trimmed.startsWith("install_requires")) {
        inInstallReqs = true
        // Check if there are deps on the same line after '='
        val afterEquals = trimmed.dropWhile(_ != '=').drop(1).trim
        if (afterEquals.nonEmpty) {
          parseSetupCfgDepLine(afterEquals, dstGraph)
        }
      } else if (inInstallReqs) {
        // Continuation lines are indented; a non-indented non-empty line ends the section
        if (line.nonEmpty && !line.startsWith(" ") && !line.startsWith("\t")) {
          inInstallReqs = false
        } else if (trimmed.nonEmpty && !trimmed.startsWith("#")) {
          parseSetupCfgDepLine(trimmed, dstGraph)
        }
      }
    }
  }

  /** Parse a single dependency line from setup.cfg's install_requires. */
  private def parseSetupCfgDepLine(line: String, dstGraph: DiffGraphBuilder): Unit = {
    val trimmed = line.trim
    trimmed match {
      case RequirementsLinePattern(name, _, version) =>
        addDependency(dstGraph, name.trim, version.trim)
      case BarePackagePattern(name) =>
        addDependency(dstGraph, name.trim, "")
      case _ => // skip
    }
  }

  private def addDependency(dstGraph: DiffGraphBuilder, name: String, version: String): Unit = {
    val node = NewDependency().name(name).version(version).dependencyGroupId(name)
    dstGraph.addNode(node)
  }
}
