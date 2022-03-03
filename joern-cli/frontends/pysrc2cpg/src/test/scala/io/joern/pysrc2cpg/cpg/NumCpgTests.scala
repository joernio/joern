package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NumCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""1""".stripMargin)

  "test num literal node properties" in {
    val literal = cpg.literal.head
    literal.code shouldBe "1"
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }
}
