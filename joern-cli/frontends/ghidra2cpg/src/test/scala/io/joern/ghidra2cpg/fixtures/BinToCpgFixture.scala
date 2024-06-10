package io.joern.ghidra2cpg.fixtures

import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File

class BinToCpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg               = Cpg.empty
  val binDirectory           = ProjectRoot.relativise("src/test/resources/testbinaries/")
  def passes(cpg: Cpg): Unit = applyDefaultOverlays(cpg)

  def buildCpgForBin(binName: String): Unit = {
    if (!cpg.graph.isClosed) {
      cpg.close()
    }
    val bin = new File(binDirectory, binName)
    cpg = frontend.execute(bin)
    passes(cpg)
  }

  override def afterAll(): Unit = {
    if (!cpg.graph.isClosed) {
      cpg.close()
    }
  }
}
