package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes.{NewDependency}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.matching.Regex

// This pass takes information out of specific CONFIG_FILE nodes in order to add DEPENDENCY nodes to the graph.
/*
example of a `requirements.txt` file that is valid for the pass:
```
click==7.1.2
Flask==1.1.2
itsdangerous==1.1.0
Jinja2==2.11.3
MarkupSafe==1.1.1
Werkzeug==1.0.1
```
 */
class DependenciesFromRequirementsTxtPass(cpg: Cpg) extends CpgPass(cpg) {
  private val logger: Logger = LoggerFactory.getLogger(classOf[DependenciesFromRequirementsTxtPass])
  override def run(dstGraph: DiffGraphBuilder): Unit = {
    cpg.configFile.filter(_.name.endsWith("requirements.txt")).foreach { node =>
      val lines = node.content.split("\n")
      lines.filter(_.matches("^[^=]+==[^=]+$")).foreach { line =>
        val keyValPattern: Regex = "^([^=]+)==([^=]+)$".r
        for (patternMatch <- keyValPattern.findAllMatchIn(line)) {
          val name    = patternMatch.group(1)
          val version = patternMatch.group(2)
          val node    = NewDependency().name(name).version(version).dependencyGroupId(name)
          dstGraph.addNode(node)
        }
      }
    }
  }
}
