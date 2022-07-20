package io.joern.c2cpg.fixtures

import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.{AstCreationPass, HeaderContentPass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.base.FileCreationPass
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.utils.ProjectRoot
import overflowdb.traversal.TraversalSource

case class TestProjectFixture(projectName: String) {

  val cpg: Cpg = Cpg.emptyCpg

  private val dirName: String =
    ProjectRoot.relativise(s"joern-cli/frontends/c2cpg/src/test/resources/testcode/$projectName")

  private val config = Config(inputPath = dirName, includePathsAutoDiscovery = false)

  new MetaDataPass(cpg, Languages.C, config.inputPath).createAndApply()
  new AstCreationPass(cpg, AstCreationPass.SourceFiles, config).createAndApply()
  new AstCreationPass(cpg, AstCreationPass.HeaderFiles, config).createAndApply()
  new HeaderContentPass(cpg, config).createAndApply()
  new CfgCreationPass(cpg).createAndApply()
  new FileCreationPass(cpg).createAndApply()

  def traversalSource: TraversalSource = TraversalSource(cpg.graph)

}
