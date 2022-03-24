package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.testfixtures.CodeToCpgFixture.codeToSystemLinebreaks
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File

class CodeToCpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val code                   = ""
  var cpg: Cpg               = _
  def passes(cpg: Cpg): Unit = applyDefaultOverlays(cpg)

  override def beforeAll(): Unit = {
    val tmpDir = writeCodeToFile(code)
    buildCpgForDir(tmpDir)
  }

  def writeCodeToFile(sourceCode: String): File = {
    X2Cpg.writeCodeToFile(codeToSystemLinebreaks(sourceCode), "semanticcpgtest", frontend.fileSuffix)
  }

  private def buildCpgForDir[T](dir: File): Unit = {
    cpg = frontend.execute(dir)
    passes(cpg)
  }

  override def afterAll(): Unit = {
    cpg.close()
  }

}

object CodeToCpgFixture {
  def codeToSystemLinebreaks(code: String): String =
    if (code.matches("(?s).*(\\r\\n).*")) { // Windows
      code.replace("\r\n", System.lineSeparator())
    } else if (code.matches("(?s).*(\\n).*")) { // Unix/Linux
      code.replace("\n", System.lineSeparator())
    } else if (code.matches("(?s).*(\\r).*")) { // Legacy mac os 9. Newer OS X use \n
      code.replace("\r", System.lineSeparator())
    } else {
      code // fallback if nothing matches.
    }
}
