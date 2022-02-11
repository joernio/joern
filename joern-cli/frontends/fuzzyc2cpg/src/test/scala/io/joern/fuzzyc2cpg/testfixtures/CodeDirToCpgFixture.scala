package io.joern.fuzzyc2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.{Base, LayerCreatorContext}
import io.shiftleft.semanticcpg.testfixtures.LanguageFrontend
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File

class CodeDirToCpgFixture extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val dir: java.io.File          = null
  var cpg: Cpg                   = _
  val frontend: LanguageFrontend = new FuzzycFrontend
  def passes(cpg: Cpg): Unit     = createEnhancements(cpg)

  override def beforeAll(): Unit = {
    buildCpgForDir(dir)
  }

  def createEnhancements(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
  }

  private def buildCpgForDir[T](dir: File): Unit = {
    cpg = frontend.execute(dir)
    passes(cpg)
  }

  override def afterAll(): Unit = {
    cpg.close()
  }

}
