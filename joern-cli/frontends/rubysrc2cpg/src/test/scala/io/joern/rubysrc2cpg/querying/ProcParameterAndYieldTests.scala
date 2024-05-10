package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import org.scalatest.Inspectors
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.*

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

        "represent the yield as a conditional with a call and return node as children" in {
          forAll(cpgs) { cpg =>
            inside(cpg.method("foo").call("<operator>.conditional").code("yield").astChildren.l) {
              case List(cond: Expression, call: Call, ret: Return) => {
                cond.code shouldBe "<nondet>"
                call.name shouldBe "b"
                call.code shouldBe "yield"
                ret.code shouldBe "yield"
              }
            }
          }
        }
      }

      "without a proc parameter" should {
        val cpg1 = code("def foo() yield end")
        val cpg2 = code("def self.foo() yield end")
        val cpgs = List(cpg1, cpg2)

        "have a call to a block parameter" in {
          forAll(cpgs)(_.call.code("yield").astChildren.isCall.code("yield").name.l shouldBe List("<proc-param-0>"))
        }

        "add a block argument" in {
          val List(param1) = cpg1.method("foo").parameter.code("&.*").l
          param1.name shouldBe "<proc-param-0>"
          param1.index shouldBe 1

          val List(param2) = cpg2.method("foo").parameter.code("&.*").l
          param2.name shouldBe "<proc-param-0>"
          param2.index shouldBe 1
        }
      }

      "with yield arguments" should {
        val cpg = code("def foo(x) yield(x) end")
        "replace the yield with a call to the block parameter with arguments" in {
          val List(call) = cpg.call.codeExact("yield(x)").astChildren.isCall.codeExact("yield(x)").l
          call.name shouldBe "<proc-param-0>"
          call.argument.code.l shouldBe List("<proc-param-0>", "x")
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
