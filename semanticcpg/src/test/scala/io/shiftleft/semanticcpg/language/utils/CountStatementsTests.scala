package io.shiftleft.semanticcpg.utils

import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CountStatementsTests extends AnyWordSpec with Matchers {

  val cpg = MockCpg()
    .withMethod("foo")
    .withCallInMethod("foo", "call1")
    .withCallInMethod("foo", "call2")
    .withCallInMethod("foo", "call3")
    .cpg

  "Class Statements" should {
    "count statements correctly" in {
      Statements.countAll(cpg) should be(3)
    }
  }
}
