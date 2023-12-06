package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.CpgPass

/** Creation of DEPENDENCY nodes from "Package.swift" files.
  */
class DependenciesPass(cpg: Cpg, config: Config) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    // TODO: gather the actual dependencies from the Package.swift file
    val dependencies: List[(String, String)] = List()
    dependencies.foreach { case (name, version) =>
      val dep = NewDependency()
        .name(name)
        .version(version)
      diffGraph.addNode(dep)
    }
  }

}
