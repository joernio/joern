// This test file has been translated from swift/test/Parse/actor.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ActorTests extends SwiftCompilerSrc2CpgSuite {

  "ActorTests" should {

    "testActor1" in {
      val testCode    = "actor MyActor1 {}"
      val cpg         = code(testCode)
      val compilerCpg = codeWithSwiftSetup(testCode)

      val List(myActor) = cpg.typeDecl.nameExact("MyActor1").l
      myActor.fullName shouldBe "Sources/main.swift:<global>.MyActor1"
      myActor.member shouldBe empty
      myActor.boundMethod.fullName.l shouldBe List(
        "Sources/main.swift:<global>.MyActor1.init:()->Sources/main.swift:<global>.MyActor1"
      )

      val List(myActorSwiftc) = compilerCpg.typeDecl.nameExact("MyActor1").l
      myActorSwiftc.fullName shouldBe "SwiftTest.MyActor1"
      myActorSwiftc.member shouldBe empty
      myActorSwiftc.boundMethod.fullName.l shouldBe List("SwiftTest.MyActor1.init:()->SwiftTest.MyActor1")
    }

    "testActor2" in {
      val testCode = """
       |actor MyActor2 {
       |  init() {}
       |  func hello() {}
       |  func foo(x: String) -> Int { return 0 }
       |}""".stripMargin
      val cpg         = code(testCode)
      val compilerCpg = codeWithSwiftSetup(testCode)

      val List(myActor) = cpg.typeDecl.nameExact("MyActor2").l
      myActor.fullName shouldBe "Sources/main.swift:<global>.MyActor2"
      val List(constructor) = myActor.method.isConstructor.l
      constructor.name shouldBe "init"
      constructor.fullName shouldBe "Sources/main.swift:<global>.MyActor2.init:()->Sources/main.swift:<global>.MyActor2"
      val List(init, hello, foo) = myActor.boundMethod.l
      init shouldBe constructor
      hello.name shouldBe "hello"
      hello.fullName shouldBe "Sources/main.swift:<global>.MyActor2.hello:()->ANY"
      foo.name shouldBe "foo"
      foo.fullName shouldBe "Sources/main.swift:<global>.MyActor2.foo:(x:Swift.String)->Swift.Int"

      val List(myActorSwiftc) = compilerCpg.typeDecl.nameExact("MyActor2").l
      myActorSwiftc.fullName shouldBe "SwiftTest.MyActor2"
      val List(constructorSwiftc) = myActorSwiftc.method.isConstructor.l
      constructorSwiftc.name shouldBe "init"
      constructorSwiftc.fullName shouldBe "SwiftTest.MyActor2.init:()->SwiftTest.MyActor2"
      val List(initSwiftc, helloSwiftc, fooSwiftc) = myActorSwiftc.boundMethod.l
      initSwiftc shouldBe constructorSwiftc
      helloSwiftc.name shouldBe "hello"
      helloSwiftc.fullName shouldBe "SwiftTest.MyActor2.hello:()->()"
      fooSwiftc.name shouldBe "foo"
      fooSwiftc.fullName shouldBe "SwiftTest.MyActor2.foo:(x:Swift.String)->Swift.Int"
    }
  }

}
