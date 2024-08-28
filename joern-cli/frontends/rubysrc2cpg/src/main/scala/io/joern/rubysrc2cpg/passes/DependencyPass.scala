package io.joern.rubysrc2cpg.passes

import flatgraph.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{ConfigFile, NewDependency}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Parses the dependencies from the `Gemfile.lock` and `Gemfile` files. This pass uses a dependency node to store the
  * Ruby Gems resolver with the `name` as `Defines.Resolver` and `version` as the URL.
  * @param cpg
  *   the graph.
  */
class DependencyPass(cpg: Cpg) extends ForkJoinParallelCpgPass[ConfigFile](cpg) {

  /** Adds all necessary initial core gems.
    */
  override def init(): Unit = {
    val diffGraph = Cpg.newDiffGraphBuilder
    DependencyPass.CORE_GEMS
      .map { coreGemName =>
        NewDependency().name(coreGemName).version(DependencyPass.CORE_GEM_VERSION)
      }
      .foreach(diffGraph.addNode)
    flatgraph.DiffGraphApplier.applyDiff(cpg.graph, diffGraph)
  }

  /** @return
    *   the Gemfiles, while preferring `Gemfile.lock` files if present.
    */
  override def generateParts(): Array[ConfigFile] = {
    val allGemfiles = cpg.configFile.name(".*Gemfile(\\.lock)?").toArray
    if (allGemfiles.count(_.name.endsWith("Gemfile.lock")) > 0) allGemfiles.filter(_.name.endsWith("Gemfile.lock"))
    else allGemfiles
  }

  override def runOnPart(builder: DiffGraphBuilder, configFile: ConfigFile): Unit = {
    if (configFile.name.endsWith("Gemfile.lock")) parseLockFile(builder, configFile)
    else parseGemfile(builder, configFile)
  }

  private def parseLockFile(builder: DiffGraphBuilder, configFile: ConfigFile): Unit = {
    var inGem       = false
    var inSpecs     = false
    var specTabSize = 0
    val specRegex   = " *([\\w_-]+) \\(([\\d.]+)\\)".r
    configFile.content.linesIterator.foreach {
      // Check if we're under the GEM header
      case "GEM" => inGem = true
      // Check if we've left the GEM header
      case "" if inGem => inGem = false
      // Check if we're under a new header
      case line if !line.isBlank && !line.charAt(0).isWhitespace => inGem = false
      // If we're under the GEM header, process line
      case gemLine if inGem =>
        val currentTabSize = gemLine.takeWhile(_.isWhitespace).length
        gemLine match {
          case remote if remote.trim.startsWith("remote:") =>
            val depNode = NewDependency()
              .name(Defines.Resolver)
              .version(remote.stripSuffix("remote:").trim)
            builder.addNode(depNode)
          case specs if specs.trim.startsWith("specs:") =>
            inSpecs = true
          case specRegex(dep, version) if inSpecs && (specTabSize == currentTabSize || specTabSize == 0) =>
            specTabSize = currentTabSize
            val depNode = NewDependency()
              .name(dep)
              .version(version)
            builder.addNode(depNode)
          case _ => // do nothing
        }
      case _ => // do nothing
    }
  }

  private def parseGemfile(builder: DiffGraphBuilder, configFile: ConfigFile): Unit = {
    val gemRegex    = "gem [\"']([\\w_-]+)[\"'][, ]*(?:[\"']([\\w.]+)[\"'])?".r
    val sourceRegex = "source [\"']([/:\\w\\d_.-]+)[\"']".r
    configFile.content.linesIterator.filterNot(_.isBlank).foreach {
      case gemRegex(dep, null) =>
        builder.addNode(
          NewDependency()
            .name(dep)
            .version("")
        )
      case gemRegex(dep, version) =>
        builder.addNode(
          NewDependency()
            .name(dep)
            .version(version)
        )
      case sourceRegex(url) =>
        builder.addNode(
          NewDependency()
            .name(Defines.Resolver)
            .version(url)
        )
      case _ => // do nothing
    }
  }

}

object DependencyPass {
  val CORE_GEM_VERSION: String = "3.0.0"
  // Scraped from: https://ruby-doc.org/stdlib-$CORE_GEM_VERSION/
  // These gems require explicit import but no entry required in `Gemsfile`
  val CORE_GEMS: Set[String] = Set(
    "abbrev",
    "base64",
    "benchmark",
    "bigdecimal",
    "bundler",
    "cgi",
    "coverage",
    "csv",
    "date",
    "dbm",
    "debug",
    "delegate",
    "did_you_mean",
    "digest",
    "drb",
    "English",
    "erb",
    "etc",
    "extmk",
    "fcntl",
    "fiddle",
    "fileutils",
    "find",
    "forwardable",
    "gdbm",
    "getoptlong",
    "io/console",
    "io/nonblock",
    "io/wait",
    "ipaddr",
    "irb",
    "json",
    "logger",
    "matrix",
    "minitest",
    "mkmf",
    "monitor",
    "mutex_m",
    "net/ftp",
    "net/http",
    "net/imap",
    "net/pop",
    "net/protocol",
    "net/smtp",
    "nkf",
    "objspace",
    "observer",
    "open-uri",
    "open3",
    "openssl",
    "optparse",
    "ostruct",
    "pathname",
    "power_assert",
    "pp",
    "prettyprint",
    "prime",
    "pstore",
    "psych",
    "pty",
    "racc",
    "racc/parser",
    "rake",
    "rbs",
    "readline",
    "readline",
    "reline",
    "resolv",
    "resolv-replace",
    "rexml",
    "rinda",
    "ripper",
    "rss",
    "rubygems",
    "securerandom",
    "set",
    "shellwords",
    "singleton",
    "socket",
    "stringio",
    "strscan",
    "syslog",
    "tempfile",
    "test-unit",
    "time",
    "timeout",
    "tmpdir",
    "tracer",
    "tsort",
    "typeprof",
    "un",
    "uri",
    "weakref",
    "win32ole",
    "yaml",
    "zlib"
  )
}
