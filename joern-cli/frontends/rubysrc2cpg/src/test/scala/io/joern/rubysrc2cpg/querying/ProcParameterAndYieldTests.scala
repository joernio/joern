package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import org.scalatest.Inspectors
import io.shiftleft.semanticcpg.language.*

class ProcParameterAndYieldTests extends RubyCode2CpgFixture with Inspectors {
  "Methods" should {
    "with a yield expression" should {
      "with a proc parameter" should {
        val cpg1 = code("def foo(&b) yield end")
        val cpg2 = code("def self.foo(&b) yield end")
        val cpgs = List(cpg1, cpg2)

        "have a single block argument" in {
          forAll(cpgs)(_.method("foo").parameter.code("&.*").name.l shouldBe List("b"))
        }

        "replace the yield with a call to the block parameter" in {
          forAll(cpgs)(_.call.code("yield").receiver.isCall.argument.code.l shouldBe List("b", "call"))
        }
      }
      "without a proc parameter" should {
        val cpg1 = code("def foo() yield end")
        val cpg2 = code("def self.foo() yield end")
        val cpgs = List(cpg1, cpg2)

        "replace the yield with a call to a block parameter" in {
          forAll(cpgs)(_.call.code("yield").receiver.isCall.argument.code.l shouldBe List("<proc-param-0>", "call"))
        }

        "add a block argument" in {
          forAll(cpgs.zipWithIndex) { (cpg, i) =>
            val List(param) = cpg.method("foo").parameter.code("&.*").l
            param.name shouldBe "<proc-param-0>"
            param.index shouldBe i
          }
        }
      }
    }

    "that don't have a yield nor a proc parameter" should {
      val cpg1 = code("def foo() end")
      val cpg2 = code("def self.foo() end")
      val cpgs = List(cpg1, cpg2)

      "not add a block argument" in {
        forAll(cpgs)(_.method("foo").parameter.code("&.*").name.l should be(empty))
      }
    }
  }

}
