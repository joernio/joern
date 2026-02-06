package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*

class LabeledParamSignatureTests extends SwiftSrc2CpgSuite {

  "LabeledParamSignatureTests" should {

    "create correct signatures for functions with labeled parameters" in {
      val cpg = code("""
          |func someFunction(argumentLabel parameterName: Int) { }
          |func someFunction(argumentLabel parameterName: String) {}
          |func someFunction(otherLabel parameterName: Int) -> String { "" }
          |func someFunction(otherLabel parameterName: Int) -> Int { print("Hello") ; return 0 }
          |func someFunction(_ parameterName: Int) -> Int { print("World"); return 0 }
          |func someFunction(parameterLabel: Int) -> Int { print("!"); return 0 }
          |
          |let i: Int = someFunction(otherLabel: 1)
          |someFunction(2)
          |someFunction(parameterLabel: 2)
          |""".stripMargin)
      val List(f1, f2, f3, f4, f5, f6) = cpg.method.nameExact("someFunction").l
      val List(c1, c2, c3)             = cpg.call.nameExact("someFunction").l

      f1.fullName shouldBe "Test0.swift:<global>.someFunction:(argumentLabel:Swift.Int)->ANY"
      f2.fullName shouldBe "Test0.swift:<global>.someFunction:(argumentLabel:Swift.String)->ANY"
      f3.fullName shouldBe "Test0.swift:<global>.someFunction:(otherLabel:Swift.Int)->Swift.String"
      f4.fullName shouldBe "Test0.swift:<global>.someFunction:(otherLabel:Swift.Int)->Swift.Int"
      f5.fullName shouldBe "Test0.swift:<global>.someFunction:(_:Swift.Int)->Swift.Int"
      f6.fullName shouldBe "Test0.swift:<global>.someFunction:(parameterLabel:Swift.Int)->Swift.Int"

      f1.signature shouldBe "(argumentLabel:Swift.Int)->ANY"
      f2.signature shouldBe "(argumentLabel:Swift.String)->ANY"
      f3.signature shouldBe "(otherLabel:Swift.Int)->Swift.String"
      f4.signature shouldBe "(otherLabel:Swift.Int)->Swift.Int"
      f5.signature shouldBe "(_:Swift.Int)->Swift.Int"
      f6.signature shouldBe "(parameterLabel:Swift.Int)->Swift.Int"

      c1.methodFullName shouldBe Defines.DynamicCallUnknownFullName
      c2.methodFullName shouldBe Defines.DynamicCallUnknownFullName
      c3.methodFullName shouldBe Defines.DynamicCallUnknownFullName

      c1.argument.argumentName shouldBe empty
      c2.argument.argumentName shouldBe empty
      c3.argument.argumentName shouldBe empty
    }

    "create correct REF edges for labeled parameters" in {
      val cpg = code("""
          |func someFunction(argumentLabel parameterName: Int) { print(parameterName) }
          |func someFunction(argumentLabel parameterName: String) { print(parameterName) }
          |func someFunction(otherLabel parameterName: Int) -> String { print(parameterName); return "" }
          |func someFunction(otherLabel parameterName: Int) -> Int { print(parameterName); return 0 }
          |func someFunction(_ parameterName: Int) -> Int { print(parameterName); return 0 }
          |func someFunction(parameterLabel: Int) -> Int { print(parameterLabel); return 0 }
          |
          |""".stripMargin)
      val List(f1, f2, f3, f4, f5, f6) = cpg.method.nameExact("someFunction").l

      f1.body.ast.isIdentifier.nameExact("parameterName").refsTo.l shouldBe f1.parameter.nameExact("parameterName").l
      f2.body.ast.isIdentifier.nameExact("parameterName").refsTo.l shouldBe f2.parameter.nameExact("parameterName").l
      f3.body.ast.isIdentifier.nameExact("parameterName").refsTo.l shouldBe f3.parameter.nameExact("parameterName").l
      f4.body.ast.isIdentifier.nameExact("parameterName").refsTo.l shouldBe f4.parameter.nameExact("parameterName").l
      f5.body.ast.isIdentifier.nameExact("parameterName").refsTo.l shouldBe f5.parameter.nameExact("parameterName").l
      f6.body.ast.isIdentifier.nameExact("parameterLabel").refsTo.l shouldBe f6.parameter.nameExact("parameterLabel").l
    }

  }

}
