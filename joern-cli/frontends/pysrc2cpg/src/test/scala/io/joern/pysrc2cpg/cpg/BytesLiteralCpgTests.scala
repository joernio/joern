package io.joern.pysrc2cpg.cpg

import io.joern.x2cpg.frontendspecific.pysrc2cpg.Constants
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class BytesLiteralCpgTests extends PySrc2CpgFixture with Matchers {
  val cpg = code("""b"123"""".stripMargin)

  "test num literal node properties" in {
    val literal = cpg.literal.head
    literal.code shouldBe "b\"123\""
    literal.typeFullName shouldBe Constants.ANY
    literal.dynamicTypeHintFullName should contain only (Constants.builtinBytesType)
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }
}
