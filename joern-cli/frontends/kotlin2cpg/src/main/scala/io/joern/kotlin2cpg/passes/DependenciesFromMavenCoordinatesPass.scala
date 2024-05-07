package io.joern.kotlin2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPass
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency

import scala.util.matching.Regex

// This pass takes a list of strings representing maven coordinates in order to add DEPENDENCY nodes to the graph.
/*
example of a sequence of coordinates that are valid for the pass:
```
org.jetbrains.kotlin:kotlin-stdlib:1.7.22
org.jetbrains.kotlin:kotlin-stdlib-common:1.7.22
org.jetbrains:annotations:13.0
org.jetbrains.kotlin:kotlin-stdlib-jdk7:1.7.22
org.apache.logging.log4j:log4j-api:2.15.0
org.springframework.boot:spring-boot-starter:3.0.5
org.springframework.boot:spring-boot:3.0.5
org.springframework:spring-core:6.0.7
org.springframework:spring-jcl:6.0.7
org.springframework:spring-context:6.0.7
```
 */
class DependenciesFromMavenCoordinatesPass(coordinates: Seq[String], cpg: Cpg) extends CpgPass(cpg) {
  private val keyValPattern: Regex = "^([^:]+):([^:]+):([^:]+)$".r

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    coordinates.foreach { coordinate =>
      for (patternMatch <- keyValPattern.findAllMatchIn(coordinate)) {
        val groupId = patternMatch.group(1)
        val name    = patternMatch.group(2)
        val version = patternMatch.group(3)
        val node    = NewDependency().name(name).version(version).dependencyGroupId(groupId)
        dstGraph.addNode(node)
      }
    }
  }
}
