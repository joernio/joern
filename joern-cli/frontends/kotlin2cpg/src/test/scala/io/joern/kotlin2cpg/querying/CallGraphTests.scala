package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CallGraphTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with simple function declaration" should {

    val cpg = code("""
        |package mypkg
        |
        |fun add(x: Int, y: Int): Int {
        |  return x + y
        |}
        |
        |fun main(args : Array<String>) {
        |  println(add(1 + 2, 3))
        |}
        |""".stripMargin)

    "should find that add is called by main" in {
      cpg.method.name("add").caller.name.toSet shouldBe Set("main")
    }

    "should find that main calls add and others" in {
      cpg.method.name("main").callee.name.toSet shouldBe Set("add", "println", "<operator>.addition")
    }

    "should find three outgoing calls for main" in {
      cpg.method.name("main").call.code.toSet shouldBe
        Set("1 + 2", "add(1 + 2, 3)", "println(add(1 + 2, 3))")
    }

    "should find one callsite for add" in {
      cpg.method.name("add").callIn.code.toSet shouldBe Set("add(1 + 2, 3)")
    }

    "should find that argument '1+2' is passed to parameter 'x'" in {
      cpg.parameter.name("x").argument.code.toSet shouldBe Set("1 + 2")
    }

    "should allow traversing from argument to formal parameter" in {
      cpg.argument.parameter.name.toSet should not be empty
    }
  }
}
