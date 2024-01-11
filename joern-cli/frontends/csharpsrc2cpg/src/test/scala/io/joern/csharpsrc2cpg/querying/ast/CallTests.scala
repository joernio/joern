package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CallTests extends CSharpCode2CpgFixture {

  "builtin calls" should {

    val cpg = code(basicBoilerplate())

    "create a call node with arguments" in {
      inside(cpg.call.nameExact("WriteLine").head) {
        case writeLine =>
          writeLine.name shouldBe "WriteLine"
        case _ => fail("Node not found!")
      }
    }

    "be resolve a method full name without the definition clearly defined" in {}

  }

}
