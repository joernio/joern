package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ParsingErrorHandlingTests extends AnyWordSpec with Matchers {

  "A cpg with a file with a parsing error and file content enabled" should {
    val code = """assert x, y
                 |x[""".stripMargin
    val cpg = Py2CpgTestContext
      .addSource(code, "test.py")
      .withEnabledFileContent(true)
      .buildCpg
    "still have a file node and the files content" in {
      val List(file) = cpg.file.name("test.py").l
      file.content shouldBe code
    }
  }

  "A cpg with a file with a parsing error and file content disabled" should {
    val cpg = Py2CpgTestContext
      .addSource("""assert x, y
          |x[""".stripMargin)
      .withEnabledFileContent(false)
      .buildCpg
    "still have a file node but no file content" in {
      val List(file) = cpg.file.name("test.py").l
      file.content shouldBe "<empty>"
    }
  }

}
