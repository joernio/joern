package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LetScopeFunctionTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple `let`" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val numbers = mutableListOf("one", "two", "three", "four", "five")
        |  numbers.map { it.length }.let { println(it) } // prints `[3, 3, 5, 4, 4]`
        |}
        |""".stripMargin)

    "should contain IDENTIFIER nodes for the `it`s with the correct TYPE_FULL_NAME set" in {
      cpg.identifier.nameExact("it").typeFullName.toSet shouldBe Set("java.util.List", "java.lang.String")
    }
  }
}
