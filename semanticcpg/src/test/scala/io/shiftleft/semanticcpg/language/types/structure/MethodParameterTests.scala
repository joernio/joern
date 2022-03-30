package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MethodParameterTests extends AnyWordSpec with Matchers {

  val cpg = MockCpg()
    .withMethod("foo")
    .cpg

  "generic cpg" should {
    "find parameters" when {
      "asking for all parameters" in {
        val args: List[MethodParameterIn] =
          cpg.method.name("foo").parameter.toList

        args.size shouldBe 1
        args.sortBy(_.order).map(_.typ.head.name) shouldBe
          List("paramtype")
      }

      "filtering by name" in {
        val queryResult: List[MethodParameterIn] =
          cpg.method.parameter.name(".*").toList
        queryResult.size shouldBe 1
      }

      "finding parameter by index" when {
        "specifying number" in {
          val args: List[MethodParameterIn] =
            cpg.method.name("foo").parameter.index(num = 1).toList

          args.size shouldBe 1
          args.head.typ.head.name shouldBe "paramtype"
        }

        "specifying index >= x" in {
          val args: List[MethodParameterIn] =
            cpg.method.name("foo").parameter.indexFrom(1).toList

          args.map(_.typ.head.name).toSet shouldBe Set("paramtype")
        }

        "specifying index <= x" in {
          val args: List[MethodParameterIn] =
            cpg.method.name("foo").parameter.indexTo(2).toList

          args.map(_.typ.head.name).toSet shouldBe Set("paramtype")
        }
      }
    }

    "find method that a MethodParameter belongs to" in {
      val methods: List[Method] =
        cpg.method.name("foo").parameter.index(num = 1).method.toList

      methods.size shouldBe 1
      methods.head.name shouldBe "foo"
    }
  }
}
