package io.joern.ghidra2cpg.fixtures

import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers._
import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File

class BinToCpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg               = Cpg.emptyCpg
  val binDirectory           = ProjectRoot.relativise("src/test/resources/testbinaries/")
  def passes(cpg: Cpg): Unit = createEnhancements(cpg)

  def createEnhancements(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    new ControlFlow().run(context)
    new TypeRelations().run(context)
    new CallGraph().run(context)
  }

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
