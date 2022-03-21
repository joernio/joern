package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RunScopeFunctionTests extends AnyFreeSpec with Matchers {
  // TODO add test case with refs to properties without `this`

  "CPG for code with simple `run` usage" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Foo(x: String) {
        |    var bar = x
        |
        |    fun printbar() {
        |        print(bar)
        |    }
        |}
        |
        |fun main() {
        |    val f = Foo("one")
        |    f.run {
        |        printbar()
        |        print("-")
        |        this.bar = "two" // you can also omit `this`
        |        printbar()
        |    }
        |} // prints `one-two`
        |""".stripMargin)

    "should contain IDENTIFIER nodes for `this` with a correctly inferred type" in {
      cpg.identifier.nameExact("this").typeFullName.toSet shouldBe Set("mypkg.Foo")
    }

    // TODO: test more of the lowering
  }
}
