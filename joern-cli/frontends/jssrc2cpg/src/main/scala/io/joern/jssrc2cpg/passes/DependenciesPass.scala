package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.utils.PackageJsonParser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.CpgPass

import java.nio.file.Paths

/** Creation of DEPENDENCY nodes from "package.json" files.
  */
class DependenciesPass(cpg: Cpg, config: Config) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val packagesJsons = SourceFiles
      .determine(config.inputPath, Set(".json"))
      .filterNot(_.contains(Defines.NodeModulesFolder))
      .filter(f =>
        f.endsWith(PackageJsonParser.PackageJsonFilename) || f.endsWith(PackageJsonParser.PackageJsonLockFilename)
      )

    val dependencies: Map[String, String] =
      packagesJsons.flatMap(p => PackageJsonParser.dependencies(Paths.get(p))).toMap

    dependencies.foreach { case (name, version) =>
      val dep = NewDependency()
        .name(name)
        .version(version)
      diffGraph.addNode(dep)
    }
  }

}
