package io.shiftleft.py2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StrCpgTests extends AnyWordSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg(
    """"abc"""".stripMargin
  )

  "test string literal node properties" in {
    val literal = cpg.literal.head
    literal.code shouldBe "abc"
    literal.lineNumber shouldBe Some(1)
    literal.columnNumber shouldBe Some(1)
  }
}
