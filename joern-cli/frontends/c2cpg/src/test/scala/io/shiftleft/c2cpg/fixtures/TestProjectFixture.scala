package io.shiftleft.c2cpg.fixtures

import io.shiftleft.c2cpg.C2Cpg.Config
import io.shiftleft.c2cpg.passes.{AstCreationPass, HeaderAstCreationPass, HeaderContentLinkerPass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.passes.base.FileCreationPass
import io.shiftleft.semanticcpg.passes.controlflow.CfgCreationPass
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.shiftleft.utils.ProjectRoot
import overflowdb.traversal.TraversalSource

case class TestProjectFixture(projectName: String) {

  val cpg: Cpg = Cpg.emptyCpg

  private val dirName: String = ProjectRoot.relativise(s"c2cpg/src/test/resources/testcode/$projectName")

  new MetaDataPass(cpg, Languages.C).createAndApply()
  new AstCreationPass(cpg, None, Config(inputPaths = Set(dirName))).createAndApply()
  new HeaderAstCreationPass(cpg, None, Config(inputPaths = Set(dirName))).createAndApply()
  new HeaderContentLinkerPass(cpg, dirName, Set.empty).createAndApply()
  new CfgCreationPass(cpg).createAndApply()
  new FileCreationPass(cpg).createAndApply()

  def traversalSource: TraversalSource = TraversalSource(cpg.graph)

}
