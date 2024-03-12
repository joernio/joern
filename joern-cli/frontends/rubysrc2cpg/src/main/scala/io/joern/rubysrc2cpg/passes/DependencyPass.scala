package io.joern.rubysrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{ConfigFile, NewDependency}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Parses the dependencies from the `Gemfile.lock` file. TODO: Approximate dependencies from Gemfile alone.
  * @param cpg
  *   the graph.
  */
class DependencyPass(cpg: Cpg) extends ForkJoinParallelCpgPass[ConfigFile](cpg) {

  override def generateParts(): Array[ConfigFile] = cpg.configFile.name(".*Gemfile\\.lock").toArray

  override def runOnPart(builder: DiffGraphBuilder, configFile: ConfigFile): Unit = {
    var inGem       = false
    var inSpecs     = false
    var specTabSize = 0
    val specRegex   = " *([\\w_]+) \\(([\\d.]+)\\)".r
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
          case remote if remote.trim.startsWith("remote:") => // do nothing
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

}
