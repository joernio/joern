package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class TypeTests extends SwiftSrc2CpgSuite {

  "TypeTests" should {

    "testClosureParsing" in {
      val cpg = code("""
        |let a: (a, b) -> c
        |let a: @MainActor (a, b) async throws -> c
        |() -> (\u{feff})
        |""".stripMargin)
      val locals = cpg.local.nameExact("a").l
      locals.map(_.typeFullName) shouldBe List("Swift.Function<(a,b)->c>", "(a, b) async throws -> c")
    }

    "testClosureSignatures" in {
      val cpg = code("""
        |simple { [] str in
        |  print("closure with empty capture list")
        |}
        |{ ()
        |  throws -> Void in }
        |{ [weak a, unowned(safe) self, b = 3] (a: Int, b: Int, _: Int) -> Int in }
        |""".stripMargin)
      val lambdaNames = cpg.method.nameNot("<global>").nameNot("single_apply").nameNot("print").name.l
      lambdaNames shouldBe List("<lambda>0", "<lambda>1", "<lambda>2")
      val List(printCall) = cpg.call.nameExact("print").l
      printCall.code shouldBe "print(\"closure with empty capture list\")"
    }

    "testOpaqueReturnTypes" in {
      val cpg =
        code("""public typealias Body = @_opaqueReturnTypeOf("$s6CatKit10pspspspspsV5cmereV6lilguyQrvp", 0) __""")
      val List(bodyAlias) = cpg.typeDecl.nameExact("Body").l
      bodyAlias.fullName shouldBe "Test0.swift:<global>.Body"
      bodyAlias.aliasTypeFullName shouldBe Some("ANY")
    }

    "testVariadics" in {
      val cpg = code("""
        |func takesVariadicFnWithGenericRet<T>(_ fn: (S...) -> T) {}
        |let _: (S...) -> Int = \.i
        |let _: (S...) -> Int = \Array.i1
        |let _: (S...) -> Int = \S.i2
        |""".stripMargin)
      val List(takesVariadicFn) = cpg.method.nameExact("takesVariadicFnWithGenericRet").l
      takesVariadicFn.fullName shouldBe "Test0.swift:<global>.takesVariadicFnWithGenericRet:(_:Swift.Function<(S...)->T>)->ANY"
      cpg.local.nameExact("<wildcard>0", "<wildcard>1", "<wildcard>2").name.l shouldBe List(
        "<wildcard>0",
        "<wildcard>1",
        "<wildcard>2"
      )
    }

  }

}
