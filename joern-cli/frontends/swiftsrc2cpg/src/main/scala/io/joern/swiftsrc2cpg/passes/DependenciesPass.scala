package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.CpgPass

import java.nio.file.Paths

/** Creation of DEPENDENCY nodes from "package.json" files.
  */
class DependenciesPass(cpg: Cpg, config: Config) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val dependencies: List[(String, String)] =
      List() // TODO: gather the actual dependencies from the Package.swift file
    dependencies.foreach { case (name, version) =>
      val dep = NewDependency()
        .name(name)
        .version(version)
      diffGraph.addNode(dep)
    }
  }

}
