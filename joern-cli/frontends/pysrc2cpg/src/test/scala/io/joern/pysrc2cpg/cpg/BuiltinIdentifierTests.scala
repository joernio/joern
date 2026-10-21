package io.joern.pysrc2cpg.cpg

import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class BuiltinIdentifierTests extends PySrc2CpgFixture with Matchers {
  "builtin identifier" should {
    val cpg = code("""def f():
        |  x = list()""".stripMargin)

    "test assignment to list exists" in {
      cpg.call.code("list = __builtins__.list").size shouldBe 1
    }

    "test type ref to list meta type exists" in {
      val typeRef = cpg.typeRef.code("__builtins__.list").head
      typeRef.typeFullName shouldBe "__builtin.list<meta>"
    }
  }
}
