package io.joern.c2cpg.fixtures

import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.passes.{AstCreationPass, HeaderContentLinkerPass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.passes.base.FileCreationPass
import io.shiftleft.semanticcpg.passes.controlflow.CfgCreationPass
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.shiftleft.utils.ProjectRoot
import overflowdb.traversal.TraversalSource

case class TestProjectFixture(projectName: String) {

  val cpg: Cpg = Cpg.emptyCpg

  private val dirName: String =
    ProjectRoot.relativise(s"joern-cli/frontends/c2cpg/src/test/resources/testcode/$projectName")

  private val config = Config(inputPaths = Set(dirName))

  new MetaDataPass(cpg, Languages.C).createAndApply()
  new AstCreationPass(cpg, AstCreationPass.SourceFiles, None, config).createAndApply()
  new AstCreationPass(cpg, AstCreationPass.HeaderFiles, None, config).createAndApply()
  new HeaderContentLinkerPass(cpg, config).createAndApply()
  new CfgCreationPass(cpg).createAndApply()
  new FileCreationPass(cpg).createAndApply()

  def traversalSource: TraversalSource = TraversalSource(cpg.graph)

}
