package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class Code2CpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs = mutable.ArrayBuffer.empty[TestCpg]

  def code(code: String): TestCpg = {
    val newCpg = new TestCpg(frontend, this).moreCode(code)
    cpgs.append(newCpg)
    newCpg
  }

  def code(code: String, fileName: String): TestCpg = {
    val newCpg = new TestCpg(frontend, this).moreCode(code, fileName)
    cpgs.append(newCpg)
    newCpg
  }

  def applyPasses(cpg: Cpg): Unit = {
    X2Cpg.applyDefaultOverlays(cpg)
  }

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
  }
}
