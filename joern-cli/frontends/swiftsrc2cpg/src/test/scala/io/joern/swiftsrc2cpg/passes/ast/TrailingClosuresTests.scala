package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class TrailingClosuresTests extends SwiftCompilerSrc2CpgSuite {

  "TrailingClosuresTests" should {

    "be correct for simple call with one trailing closure in static function" in {
      val testCode =
        """
          |struct Controller {
          |  static func boot(routes: Builder) {
          |    routes.get("find") { req async throws -> User in
          |      return User()
          |    }
          |  }
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(getCall) = cpg.call.nameExact("get").l
      val List(arg1)    = getCall.arguments(1).isLiteral.l
      arg1.code shouldBe "\"find\""
      val List(arg2) = getCall.arguments(2).isMethodRef.l
      arg2.code shouldBe "<lambda>0"
      arg2.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.Controller.boot.<lambda>0:(ANY)->User"
    }

    "be correct for simple call with two trailing closures where the last one is named" in {
      val testCode =
        """
          |struct Controller {
          |  func boot(routes: Builder) {
          |    routes.get("find") { req async throws -> User in
          |      return User()
          |    } onFailure: { req async throws -> Error in
          |      return Abort(.internalServerError)
          |    }
          |  }
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(getCall) = cpg.call.nameExact("get").l
      val List(arg1)    = getCall.arguments(1).isLiteral.l
      arg1.code shouldBe "\"find\""
      val List(arg2) = getCall.arguments(2).isMethodRef.l
      arg2.code shouldBe "<lambda>0"
      arg2.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.Controller.boot.<lambda>0:(ANY)->User"
      val List(arg3) = getCall.arguments(3).isMethodRef.l
      arg3.code shouldBe "<lambda>1"
      arg3.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.Controller.boot.<lambda>1:(ANY)->Error"
    }

    "be correct for simple call with two trailing closures where the last one is named in static function" in {
      val testCode =
        """
          |struct Controller {
          |  static func boot(routes: Builder) {
          |    routes.get("find") { req async throws -> User in
          |      return User()
          |    } onFailure: { req async throws -> Error in
          |      return Abort(.internalServerError)
          |    }
          |  }
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(getCall) = cpg.call.nameExact("get").l
      val List(arg1)    = getCall.arguments(1).isLiteral.l
      arg1.code shouldBe "\"find\""
      val List(arg2) = getCall.arguments(2).isMethodRef.l
      arg2.code shouldBe "<lambda>0"
      arg2.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.Controller.boot.<lambda>0:(ANY)->User"
      val List(arg3) = getCall.arguments(3).isMethodRef.l
      arg3.code shouldBe "<lambda>1"
      arg3.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.Controller.boot.<lambda>1:(ANY)->Error"
    }

    "be correct for simple call with a trailing closure to an extension method" in {
      val testCode =
        """
          |extension String {
          |  func withPrefix(_ prefix: String, transform: (String) -> String) -> String {
          |    return transform(prefix + self)
          |  }
          |}
          |
          |func main() {
          |  let name = "Boimler"
          |  let result = name.withPrefix("Hi ") { value in
          |    value.uppercased()
          |  }
          |  print(result)
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(withPrefixCall) = cpg.call.nameExact("withPrefix").l
      withPrefixCall.methodFullName shouldBe "SwiftTest.Swift.String<extension>.withPrefix:(_:Swift.String,transform:(Swift.String)->Swift.String)->Swift.String"
      withPrefixCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      withPrefixCall._methodViaCallOut.fullName.loneElement shouldBe withPrefixCall.methodFullName
      val List(arg1) = withPrefixCall.arguments(1).isLiteral.l
      arg1.code shouldBe "\"Hi \""
      val List(arg2) = withPrefixCall.arguments(2).isMethodRef.l
      arg2.code shouldBe "<lambda>0"
      arg2.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.main.<lambda>0:(Swift.String)->Swift.String"
    }

    "be correct for static call with a trailing closure" in {
      val testCode =
        """
          |struct Helper {
          |  static func map(_ x: Int, transform: (Int) -> Int) -> Int {
          |    return transform(x)
          |  }
          |}
          |
          |func main() {
          |  let result = Helper.map(41) { value in
          |    return value + 1
          |  }
          |  print(result)
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(withPrefixCall) = cpg.call.nameExact("map").l
      withPrefixCall.methodFullName shouldBe "SwiftTest.Helper.map:(_:Swift.Int,transform:(Swift.Int)->Swift.Int)->Swift.Int"
      withPrefixCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      withPrefixCall._methodViaCallOut.fullName.loneElement shouldBe withPrefixCall.methodFullName
      val List(arg1) = withPrefixCall.arguments(1).isLiteral.l
      arg1.code shouldBe "41"
      val List(arg2) = withPrefixCall.arguments(2).isMethodRef.l
      arg2.code shouldBe "<lambda>0"
      arg2.referencedMethod.fullName shouldBe "Sources/main.swift:<global>.main.<lambda>0:(Swift.Int)->Swift.Int"
    }

  }

}
