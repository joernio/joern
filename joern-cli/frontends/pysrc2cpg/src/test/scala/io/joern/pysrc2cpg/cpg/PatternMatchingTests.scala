package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language._

class PatternMatchingTests extends PySrc2CpgFixture() {
  "pattern matching" should {
    "have" in {
      val cpg = code("""match [1, 2]:
          |  case [a, b]:
          |    pass
          |""".stripMargin)
      cpg.all.label("UNKNOWN").head.property("CODE") shouldBe
        """match [1, 2]:
          |  case [a, b]:
          |    pass""".stripMargin
    }
  }

}
