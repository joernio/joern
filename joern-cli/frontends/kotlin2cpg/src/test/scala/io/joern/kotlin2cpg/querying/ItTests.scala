package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ItTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple `it`" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val foo = mutableListOf("one", "two", "three", "four", "five")
        |  val bar = foo.map { it.length }
        |  println(bar)
        |}
        |""".stripMargin)

    "should contain IDENTIFIER nodes for the `it`s with the correct TYPE_FULL_NAMEs set" in {
      cpg.identifier.nameExact("it").typeFullName.toSet shouldBe Set("java.lang.String")
    }

    // TODO: test that locals are created for the `it`s
  }
}
