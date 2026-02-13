package io.joern.pysrc2cpg.cpg

import io.shiftleft.semanticcpg.language.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class ParsingErrorHandlingTestsWithFileContent extends PySrc2CpgFixture(withFileContent = true) with Matchers {

  "A cpg with a file with a parsing error and file content enabled" should {
    val code_ = """assert x, y
                 |x[""".stripMargin
    val cpg = code(code_, "test.py")
    "still have a file node and the files content" in {
      val List(file) = cpg.file.name("test.py").l
      file.content.shouldBe(code_)
    }
  }

}

class ParsingErrorHandlingTestsWithoutFileContent extends PySrc2CpgFixture(withFileContent = false) with Matchers {

  "A cpg with a file with a parsing error and file content disabled" should {
    val cpg = code(
      """assert x, y
          |x[""".stripMargin,
      "test.py"
    )
    "still have a file node but no file content" in {
      val List(file) = cpg.file.name("test.py").l
      file.content.shouldBe("<empty>")
    }
  }

}
