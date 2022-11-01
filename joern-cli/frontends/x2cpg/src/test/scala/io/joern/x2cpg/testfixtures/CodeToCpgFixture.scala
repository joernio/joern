package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File

@deprecated("Please use Code2CpgFixture instead.", "2022-04-25")
class CodeToCpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val code                   = ""
  var cpg: Cpg               = _
  def passes(cpg: Cpg): Unit = applyDefaultOverlays(cpg)

  override def beforeAll(): Unit = {
    val tmpDir = writeCodeToFile(code)
    buildCpgForDir(tmpDir)
  }

  def writeCodeToFile(sourceCode: String): File = {
    X2Cpg.writeCodeToFile(sourceCode, "semanticcpgtest", frontend.fileSuffix)
  }

  private def buildCpgForDir[T](dir: File): Unit = {
    cpg = frontend.execute(dir)
    passes(cpg)
  }

  override def afterAll(): Unit = {
    cpg.close()
  }

}
