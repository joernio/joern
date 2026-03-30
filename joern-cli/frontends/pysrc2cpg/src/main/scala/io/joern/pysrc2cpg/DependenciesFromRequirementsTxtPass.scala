package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.{NewDependency}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.matching.Regex

/** This pass takes information out of specific CONFIG_FILE nodes in order to add DEPENDENCY nodes to the graph.
  *
  * Supports:
  *   - requirements.txt (with all PEP 440 version specifiers, extras, environment markers)
  *   - pyproject.toml (PEP 621 `[project.dependencies]` and Poetry `[tool.poetry.dependencies]`)
  *   - setup.cfg (`[options] install_requires`)
  */
class DependenciesFromRequirementsTxtPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[DependenciesFromRequirementsTxtPass])

  /** Regex for requirements.txt lines: package name (with optional extras), optional version specifier, optional env
    * marker. Supports: ==, >=, <=, ~=, !=, >, < specifiers.
    */
  private val RequirementsLinePattern: Regex =
    """^\s*([A-Za-z0-9][\w.\-]*)(?:\[[^\]]*\])?\s*(?:(~=|==|!=|>=|<=|>|<)\s*([^\s;,#]+))?\s*(?:;.*)?(?:#.*)?$""".r

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    cpg.configFile.foreach { node =>
      val name = node.name
      if (name.endsWith("requirements.txt")) {
        parseRequirementsTxt(node.content, dstGraph)
      } else if (name.endsWith("pyproject.toml")) {
        parsePyprojectToml(node.content, dstGraph)
      } else if (name.endsWith("setup.cfg")) {
        parseSetupCfg(node.content, dstGraph)
      }
    }
  }

  private def parseRequirementsTxt(content: String, dstGraph: DiffGraphBuilder): Unit = {
    content.split("\n").foreach { rawLine =>
      val line = rawLine.trim
      // Skip empty lines, comments, includes (-r, -c), and option flags (--)
      if (line.nonEmpty && !line.startsWith("#") && !line.startsWith("-r ") && !line.startsWith("-c ") && !line
          .startsWith("--")) {
        line match {
          case RequirementsLinePattern(pkgName, specifier, version) =>
            val depVersion = Option(version).getOrElse("")
            val dep        = NewDependency().name(pkgName.trim).version(depVersion).dependencyGroupId(pkgName.trim)
            dstGraph.addNode(dep)
          case _ => // skip lines that don't match (e.g., URLs, editable installs)
        }
      }
    }
  }

  private def parsePyprojectToml(content: String, dstGraph: DiffGraphBuilder): Unit = {
    // Try PEP 621 [project] dependencies first
    parsePep621Dependencies(content, dstGraph)
    // Try Poetry [tool.poetry.dependencies]
    parsePoetryDependencies(content, dstGraph)
  }

  /** Parse PEP 621 style: [project] dependencies = ["flask>=2.0", "requests"] */
  private def parsePep621Dependencies(content: String, dstGraph: DiffGraphBuilder): Unit = {
    val lines      = content.split("\n")
    var inProject  = false
    var inDepArray = false
    val depLines   = scala.collection.mutable.ArrayBuffer[String]()

    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
        if (inDepArray) inDepArray = false
        inProject = trimmed == "[project]"
      } else if (inProject && trimmed.startsWith("dependencies")) {
        // Could be single-line or multi-line array
        val afterEquals = trimmed.dropWhile(_ != '=').drop(1).trim
        if (afterEquals.startsWith("[")) {
          if (afterEquals.contains("]")) {
            // Single-line array
            extractQuotedStrings(afterEquals).foreach(depLines.addOne)
          } else {
            inDepArray = true
          }
        }
      } else if (inDepArray) {
        if (trimmed.startsWith("]")) {
          inDepArray = false
        } else {
          extractQuotedStrings(trimmed).foreach(depLines.addOne)
        }
      }
    }

    depLines.foreach(depStr => parseRequirementString(depStr, dstGraph))
  }

  /** Parse Poetry style: [tool.poetry.dependencies] flask = "^2.0" requests = {version = "^2.28", optional = true} */
  private def parsePoetryDependencies(content: String, dstGraph: DiffGraphBuilder): Unit = {
    val lines        = content.split("\n")
    var inPoetryDeps = false

    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
        inPoetryDeps = trimmed == "[tool.poetry.dependencies]"
      } else if (inPoetryDeps && trimmed.contains("=") && !trimmed.startsWith("#")) {
        val parts   = trimmed.split("=", 2)
        val pkgName = parts(0).trim
        if (pkgName != "python" && pkgName.nonEmpty) {
          val versionPart = parts(1).trim.stripPrefix("\"").stripSuffix("\"")
          // Handle table syntax {version = "^1.0", ...}
          val version = if (versionPart.startsWith("{")) {
            val versionMatch = """version\s*=\s*"([^"]+)"""".r.findFirstMatchIn(versionPart)
            versionMatch.map(_.group(1)).getOrElse("")
          } else {
            versionPart
          }
          val dep = NewDependency().name(pkgName).version(version).dependencyGroupId(pkgName)
          dstGraph.addNode(dep)
        }
      }
    }
  }

  /** Parse setup.cfg [options] install_requires = flask>=2.0 requests */
  private def parseSetupCfg(content: String, dstGraph: DiffGraphBuilder): Unit = {
    val lines            = content.split("\n")
    var inOptions        = false
    var inInstallReqs    = false
    var foundFirstIndent = false

    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
        inOptions = trimmed == "[options]"
        inInstallReqs = false
        foundFirstIndent = false
      } else if (inOptions && trimmed.startsWith("install_requires")) {
        inInstallReqs = true
        // Check if there are deps on the same line after '='
        val afterEquals = trimmed.dropWhile(_ != '=').drop(1).trim
        if (afterEquals.nonEmpty) {
          parseRequirementString(afterEquals, dstGraph)
        }
      } else if (inInstallReqs) {
        // Continuation lines must be indented
        if (line.nonEmpty && (line.startsWith(" ") || line.startsWith("\t"))) {
          if (trimmed.nonEmpty && !trimmed.startsWith("#")) {
            parseRequirementString(trimmed, dstGraph)
          }
        } else {
          inInstallReqs = false
        }
      }
    }
  }

  /** Parse a single requirement string like "flask>=2.0" or "requests" into a dependency node. */
  private def parseRequirementString(reqStr: String, dstGraph: DiffGraphBuilder): Unit = {
    val cleaned = reqStr.stripPrefix("\"").stripSuffix("\"").stripPrefix("'").stripSuffix("'").trim
      .replaceAll(",\\s*$", "") // strip trailing comma
    if (cleaned.nonEmpty) {
      cleaned match {
        case RequirementsLinePattern(pkgName, _, version) =>
          val depVersion = Option(version).getOrElse("")
          val dep        = NewDependency().name(pkgName.trim).version(depVersion).dependencyGroupId(pkgName.trim)
          dstGraph.addNode(dep)
        case _ => // skip
      }
    }
  }

  private def extractQuotedStrings(s: String): Seq[String] = {
    """"([^"]+)"|'([^']+)'""".r.findAllMatchIn(s).map { m =>
      Option(m.group(1)).getOrElse(m.group(2))
    }.toSeq
  }
}
