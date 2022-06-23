package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.utils.PackageJsonParser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.SimpleCpgPass

import java.nio.file.Paths

class DependenciesPass(cpg: Cpg, config: Config) extends SimpleCpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val packagesJsons = SourceFiles
      .determine(config.inputPath, Set(".json"))
      .toSet
      .filter(f =>
        f.endsWith(PackageJsonParser.PACKAGE_JSON_FILENAME) || f.endsWith(PackageJsonParser.PACKAGE_JSON_LOCK_FILENAME)
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
