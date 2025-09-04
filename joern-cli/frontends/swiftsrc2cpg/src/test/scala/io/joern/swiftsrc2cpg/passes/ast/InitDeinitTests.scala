// This test file has been translated from swift/test/Parse/init_deinit.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class InitDeinitTests extends SwiftCompilerSrc2CpgSuite {

  "InitDeinitTests" should {

    "testInitDeinit2" in {
      val testCode = """
        |struct FooStructConstructorA {
        |  init() {}
        |}
        |""".stripMargin
      val cpg                = code(testCode)
      val compilerCpg        = codeWithSwiftSetup(testCode)
      val List(constructorA) = cpg.method.isConstructor.l
      val List(constructorB) = compilerCpg.method.isConstructor.l

      constructorA.methodReturn.typeFullName shouldBe "Sources/main.swift:<global>.FooStructConstructorA"
      constructorA.methodReturn.dynamicTypeHintFullName shouldBe Seq(
        "Sources/main.swift:<global>.FooStructConstructorA"
      )
      constructorA.fullName shouldBe "Sources/main.swift:<global>.FooStructConstructorA.init:()->Sources/main.swift:<global>.FooStructConstructorA"

      constructorB.methodReturn.typeFullName shouldBe "SwiftTest.FooStructConstructorA"
      constructorB.methodReturn.dynamicTypeHintFullName shouldBe Seq("SwiftTest.FooStructConstructorA")
      constructorB.fullName shouldBe "SwiftTest.FooStructConstructorA.init:()->SwiftTest.FooStructConstructorA"

      val List(paramA) = constructorA.parameter.l
      val List(paramB) = constructorB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.FooStructConstructorA"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.FooStructConstructorA"
    }

    "testInitDeinit4" in {
      val testCode = """
        |struct FooStructConstructorA {
        |  init?() { self.init() }
        |}
        |""".stripMargin
      val cpg                = code(testCode)
      val compilerCpg        = codeWithSwiftSetup(testCode)
      val List(constructorA) = cpg.method.isConstructor.l
      val List(constructorB) = compilerCpg.method.isConstructor.l

      constructorA.methodReturn.typeFullName shouldBe "Sources/main.swift:<global>.FooStructConstructorA"
      constructorA.methodReturn.dynamicTypeHintFullName shouldBe Seq(
        "Sources/main.swift:<global>.FooStructConstructorA"
      )
      constructorA.fullName shouldBe "Sources/main.swift:<global>.FooStructConstructorA.init:()->Sources/main.swift:<global>.FooStructConstructorA"

      constructorB.methodReturn.typeFullName shouldBe "Swift.Optional"
      constructorB.methodReturn.dynamicTypeHintFullName shouldBe Seq("Swift.Optional")
      constructorB.fullName shouldBe "SwiftTest.FooStructConstructorA.init:()->Swift.Optional"

      val List(paramA) = constructorA.parameter.l
      val List(paramB) = constructorB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.FooStructConstructorA"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.FooStructConstructorA"
    }

    "testInitDeinit7" in {
      val testCode = """
        |class FooStructDeinitializerA {
        |  deinit {}
        |}
        |""".stripMargin
      val cpg           = code(testCode)
      val compilerCpg   = codeWithSwiftSetup(testCode)
      val List(deinitA) = cpg.method.nameExact("deinit").l
      val List(deinitB) = compilerCpg.method.nameExact("deinit").l

      deinitA.fullName shouldBe "Sources/main.swift:<global>.FooStructDeinitializerA.deinit:()->ANY"
      deinitB.fullName shouldBe "SwiftTest.FooStructDeinitializerA.deinit:()"

      val List(paramA) = deinitA.parameter.l
      val List(paramB) = deinitB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.FooStructDeinitializerA"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.FooStructDeinitializerA"
    }

    "testInitDeinit15" in {
      val testCode = """
        |enum BarUnion {
        |  init() { self.init() }
        |}
        |""".stripMargin
      val cpg                = code(testCode)
      val compilerCpg        = codeWithSwiftSetup(testCode)
      val List(constructorA) = cpg.method.isConstructor.l
      val List(constructorB) = compilerCpg.method.isConstructor.l

      constructorA.methodReturn.typeFullName shouldBe "Sources/main.swift:<global>.BarUnion"
      constructorA.methodReturn.dynamicTypeHintFullName shouldBe Seq("Sources/main.swift:<global>.BarUnion")
      constructorA.fullName shouldBe "Sources/main.swift:<global>.BarUnion.init:()->Sources/main.swift:<global>.BarUnion"

      constructorB.methodReturn.typeFullName shouldBe "SwiftTest.BarUnion"
      constructorB.methodReturn.dynamicTypeHintFullName shouldBe Seq("SwiftTest.BarUnion")
      constructorB.fullName shouldBe "SwiftTest.BarUnion.init:()->SwiftTest.BarUnion"

      val List(paramA) = constructorA.parameter.l
      val List(paramB) = constructorB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.BarUnion"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.BarUnion"
    }

    "testInitDeinit17" in {
      val testCode = """
        |class BarClass {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin
      val cpg                = code(testCode)
      val compilerCpg        = codeWithSwiftSetup(testCode)
      val List(constructorA) = cpg.method.isConstructor.l
      val List(constructorB) = compilerCpg.method.isConstructor.l

      constructorA.methodReturn.typeFullName shouldBe "Sources/main.swift:<global>.BarClass"
      constructorA.methodReturn.dynamicTypeHintFullName shouldBe Seq("Sources/main.swift:<global>.BarClass")
      constructorA.fullName shouldBe "Sources/main.swift:<global>.BarClass.init:()->Sources/main.swift:<global>.BarClass"

      constructorB.methodReturn.typeFullName shouldBe "SwiftTest.BarClass"
      constructorB.methodReturn.dynamicTypeHintFullName shouldBe Seq("SwiftTest.BarClass")
      constructorB.fullName shouldBe "SwiftTest.BarClass.init:()->SwiftTest.BarClass"

      val List(paramA) = constructorA.parameter.l
      val List(paramB) = constructorB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.BarClass"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.BarClass"

      val List(deinitA) = cpg.method.nameExact("deinit").l
      val List(deinitB) = compilerCpg.method.nameExact("deinit").l

      deinitA.fullName shouldBe "Sources/main.swift:<global>.BarClass.deinit:()->ANY"
      deinitB.fullName shouldBe "SwiftTest.BarClass.deinit:()"

      val List(deinitParamA) = deinitA.parameter.l
      val List(deinitParamB) = deinitB.parameter.l

      deinitParamA.index shouldBe 0
      deinitParamA.order shouldBe 0
      deinitParamA.name shouldBe "self"
      deinitParamA.typeFullName shouldBe "Sources/main.swift:<global>.BarClass"

      deinitParamB.index shouldBe 0
      deinitParamB.order shouldBe 0
      deinitParamB.name shouldBe "self"
      deinitParamB.typeFullName shouldBe "SwiftTest.BarClass"
    }

    "testInitDeinit18" in {
      val testCode =
        """
          |class BarClass {
          |  init(a: Int) {}
          |}
          |""".stripMargin
      val cpg                = code(testCode)
      val compilerCpg        = codeWithSwiftSetup(testCode)
      val List(constructorA) = cpg.method.isConstructor.l
      val List(constructorB) = compilerCpg.method.isConstructor.l

      constructorA.methodReturn.typeFullName shouldBe "Sources/main.swift:<global>.BarClass"
      constructorA.methodReturn.dynamicTypeHintFullName shouldBe Seq("Sources/main.swift:<global>.BarClass")
      constructorA.fullName shouldBe "Sources/main.swift:<global>.BarClass.init:(a:Swift.Int)->Sources/main.swift:<global>.BarClass"

      constructorB.methodReturn.typeFullName shouldBe "SwiftTest.BarClass"
      constructorB.methodReturn.dynamicTypeHintFullName shouldBe Seq("SwiftTest.BarClass")
      constructorB.fullName shouldBe "SwiftTest.BarClass.init:(a:Swift.Int)->SwiftTest.BarClass"

      val List(paramA, xA) = constructorA.parameter.l
      val List(paramB, xB) = constructorB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.BarClass"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.BarClass"

      xA.index shouldBe 1
      xA.order shouldBe 1
      xA.name shouldBe "a"
      xA.typeFullName shouldBe "Swift.Int"

      xB.index shouldBe 1
      xB.order shouldBe 1
      xB.name shouldBe "a"
      xB.typeFullName shouldBe "Swift.Int"
    }

    "testInitDeinit19" in {
      val testCode = """
        |protocol BarProtocol {
        |  init()
        |}
        |""".stripMargin
      val cpg                = code(testCode)
      val compilerCpg        = codeWithSwiftSetup(testCode)
      val List(constructorA) = cpg.method.isConstructor.l
      val List(constructorB) = compilerCpg.method.isConstructor.l

      constructorA.methodReturn.typeFullName shouldBe "Sources/main.swift:<global>.BarProtocol"
      constructorA.methodReturn.dynamicTypeHintFullName shouldBe Seq("Sources/main.swift:<global>.BarProtocol")
      constructorA.fullName shouldBe "Sources/main.swift:<global>.BarProtocol.init:()->Sources/main.swift:<global>.BarProtocol"

      // Swift initializers conceptually return `Self`. For a protocol requirement, `Self` is not a concrete type;
      // it is an abstract, generic type parameter constrained to the protocol.
      // In the compiler’s canonical form (and thus in your CPG), that abstract `Self` is represented as the first
      // generic parameter `A` (think: `<A where A: SwiftTest.BarProtocol>`).
      //
      // Returning `SwiftTest.BarProtocol` would imply an existential (boxed) value,
      // but a protocol initializer constructs the concrete conforming type, not the existential.
      // Hence, the signature is `()->A`, matching Swift’s `init` => `Self`.
      constructorB.methodReturn.typeFullName shouldBe "A"
      constructorB.methodReturn.dynamicTypeHintFullName shouldBe Seq("A")
      constructorB.fullName shouldBe "SwiftTest.BarProtocol.init:()->A"

      val List(paramA) = constructorA.parameter.l
      val List(paramB) = constructorB.parameter.l

      paramA.index shouldBe 0
      paramA.order shouldBe 0
      paramA.name shouldBe "self"
      paramA.typeFullName shouldBe "Sources/main.swift:<global>.BarProtocol"

      paramB.index shouldBe 0
      paramB.order shouldBe 0
      paramB.name shouldBe "self"
      paramB.typeFullName shouldBe "SwiftTest.BarProtocol"
    }
  }

}
