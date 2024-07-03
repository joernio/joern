// This test file has been translated from swift/test/Parse/actor.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ActorTests extends AstSwiftSrc2CpgSuite {

  "ActorTests" should {

    "testActor1" in {
      val cpg            = code("actor MyActor1 {}")
      val List(myActor1) = cpg.typeDecl.nameExact("MyActor1").l
      myActor1.fullName shouldBe "Test0.swift:<global>:MyActor1"
      myActor1.member shouldBe empty
      myActor1.boundMethod shouldBe empty
    }

    "testActor2" in {
      val cpg = code("""
        |actor MyActor2 {
        |  init() {}
        |  func hello() {}
        |}""".stripMargin)
      val List(myActor2) = cpg.typeDecl.nameExact("MyActor2").l
      myActor2.fullName shouldBe "Test0.swift:<global>:MyActor2"
      myActor2.member.name.l shouldBe List("hello")
      val List(constructor) = myActor2.method.isConstructor.l
      constructor.name shouldBe "init"
      constructor.fullName shouldBe "Test0.swift:<global>:MyActor2:init:Test0.swift:<global>:MyActor2()"
      val List(hello) = myActor2.boundMethod.l
      hello.name shouldBe "hello"
      hello.fullName shouldBe "Test0.swift:<global>:MyActor2:hello:ANY()"
    }
  }

}
