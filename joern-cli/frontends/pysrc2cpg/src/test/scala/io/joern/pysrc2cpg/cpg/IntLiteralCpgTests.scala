package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.{Constants, Py2CpgTestContext}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IntLiteralCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg("""1""".stripMargin)

  "test int literal node properties" in {
    val literal = cpg.literal.head
    literal.code shouldBe "1"
    literal.typeFullName shouldBe Constants.ANY
    literal.dynamicTypeHintFullName should contain only (Constants.builtinIntType)
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }
}
