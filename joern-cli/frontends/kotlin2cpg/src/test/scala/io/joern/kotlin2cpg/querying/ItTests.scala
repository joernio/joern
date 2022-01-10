package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ItTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple `it` usage and straightforward type inference situation" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val foo = mutableListOf("one", "two", "three", "four", "five")
        |  val bar = foo.map { it.length }
        |  println(bar)
        |}
        |""".stripMargin)

    "should contain IDENTIFIER nodes for the `it`s with the correct types inferred" in {
      cpg.identifier.nameExact("it").typeFullName.toSet shouldBe Set("kotlin.String")
    }

    // TODO: test that locals are created for the `it`s
  }
}
