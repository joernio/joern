package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.joern.x2cpg.testfixtures.CodeToCpgFixture.codeToSystemLinebreaks
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{File, PrintWriter}
import java.nio.file.Files

class CodeToCpgFixture(val frontend: LanguageFrontend) extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val code                   = ""
  var cpg: Cpg               = _
  def passes(cpg: Cpg): Unit = createEnhancements(cpg)

  override def beforeAll(): Unit = {
    val tmpDir = writeCodeToFile(code)
    buildCpgForDir(tmpDir)
  }

  def createEnhancements(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    new ControlFlow().run(context)
    new TypeRelations().run(context)
    new CallGraph().run(context)
  }

  private def buildCpgForDir[T](dir: File): Unit = {
    cpg = frontend.execute(dir)
    passes(cpg)
  }

  protected def writeCodeToFile(sourceCode: String): File = {
    val tmpDir = Files.createTempDirectory("semanticcpgtest").toFile
    tmpDir.deleteOnExit()
    val codeFile = File.createTempFile("Test", frontend.fileSuffix, tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(codeToSystemLinebreaks(sourceCode)); close() }
    tmpDir
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
