package io.joern.fuzzyc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.fuzzyc2cpg.passes.{AstCreationPass, StubRemovalPass}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.base.FileCreationPass
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.utils.ProjectRoot
import io.joern.x2cpg.SourceFiles
import overflowdb.traversal.TraversalSource

case class CpgTestFixture(projectName: String) {

  val cpg: Cpg     = Cpg.emptyCpg
  val dirName      = ProjectRoot.relativise(s"joern-cli/frontends/fuzzyc2cpg/src/test/resources/testcode/$projectName")
  val keyPoolFile1 = new IntervalKeyPool(1001, 2000)
  val filenames    = SourceFiles.determine(Set(dirName), Set(".c"))

  new MetaDataPass(cpg, Languages.C).createAndApply()
  new AstCreationPass(filenames, cpg, keyPoolFile1).createAndApply()
  if (cpg.method.nonEmpty) {
    new CfgCreationPass(cpg).createAndApply()
  }
  new StubRemovalPass(cpg).createAndApply()
  new FileCreationPass(cpg).createAndApply()

  def traversalSource = TraversalSource(cpg.graph)

}
