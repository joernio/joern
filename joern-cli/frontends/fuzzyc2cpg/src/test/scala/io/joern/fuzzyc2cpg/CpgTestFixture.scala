package io.joern.fuzzyc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.fuzzyc2cpg.passes.{AstCreationPass, StubRemovalPass}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.base.FileCreationPass
import io.shiftleft.semanticcpg.passes.controlflow.CfgCreationPass
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.shiftleft.utils.ProjectRoot
import io.shiftleft.x2cpg.SourceFiles
import overflowdb.traversal.TraversalSource

case class CpgTestFixture(projectName: String) {

  val cpg: Cpg = Cpg.emptyCpg

  private val dirName: String =
    ProjectRoot.relativise(s"joern-cli/frontends/fuzzyc2cpg/src/test/resources/testcode/$projectName")
  private val keyPoolFile: IntervalKeyPool = new IntervalKeyPool(1001, 2000)
  private val filenames: List[String] = SourceFiles.determine(Set(dirName), Set(".c"))

  new MetaDataPass(cpg, Languages.C).createAndApply()
  new AstCreationPass(filenames, cpg, keyPoolFile).createAndApply()
  if (cpg.method.nonEmpty) {
    new CfgCreationPass(cpg).createAndApply()
  }
  new StubRemovalPass(cpg).createAndApply()
  new FileCreationPass(cpg).createAndApply()

  def traversalSource: TraversalSource = TraversalSource(cpg.graph)

}
