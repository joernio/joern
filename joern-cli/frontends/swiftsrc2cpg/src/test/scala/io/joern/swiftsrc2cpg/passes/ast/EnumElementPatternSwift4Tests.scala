// This test file has been translated from swift/test/Parse/enum_element_pattern_swift4.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*

class EnumElementPatternSwift4Tests extends SwiftSrc2CpgSuite {

  "EnumElementPatternSwift4Tests" should {

    "testEnumElementPatternSwift42" in {
      val cpg = code("""
        |enum E {
        |  case A, B, C, D
        |  static func testE(e: E) {
        |    switch e {
        |    case A<UndefinedTy>():
        |      break
        |    case B<Int>():
        |      break
        |    default:
        |      break;
        |    }
        |  }
        |}
        |""".stripMargin)
      val enumEs = cpg.typeDecl.nameExact("E").fullName.l
      enumEs should contain("Test0.swift:<global>.E")
      val List(testE) = cpg.method.nameExact("testE").l
      testE.fullName shouldBe "Test0.swift:<global>.E.testE:(e:E)->ANY"
      val List(switchStruct) = testE.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStruct.code should startWith("switch e")
    }

    "testEnumElementPatternSwift43" in {
      val cpg = code("""
        |func testE(e: E) {
        |  switch e {
        |  case E.A<UndefinedTy>():
        |    break
        |  case E.B<Int>():
        |    break
        |  case .C():
        |    break
        |  case .D(let payload):
        |    let _: () = payload
        |    break
        |  default:
        |    break
        |  }
        |  guard
        |    case .C() = e,
        |    case .D(let payload) = e
        |  else { return }
        |}
        |""".stripMargin)
      val List(testE) = cpg.method.nameExact("testE").l
      testE.fullName shouldBe "Test0.swift:<global>.testE:(e:E)->ANY"
      val List(switchStruct) = testE.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStruct.code should startWith("switch e")
      val guardIfs = testE.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      guardIfs.code.l.exists(_.startsWith("guard")) shouldBe true
    }

    "testEnumElementPatternSwift44" in {
      val cpg = code("""
        |extension E : Error {}
        |      func canThrow() throws {
        |  throw E.A
        |}
        |""".stripMargin)
      val List(canThrow) = cpg.method.nameExact("canThrow").l
      canThrow.fullName shouldBe "Test0.swift:<global>.canThrow:()->ANY"
      cpg.call.code.l should contain("E.A")
    }

    "testEnumElementPatternSwift45" in {
      val cpg = code("""
        |do {
        |  try canThrow()
        |      } catch E.A() {
        |  // ..
        |      } catch E.B(let payload) {
        |  let _: () = payload
        |}
        |""".stripMargin)
      val List(tryStruct) = cpg.controlStructure.controlStructureType(ControlStructureTypes.TRY).l
      tryStruct.code should startWith("do {")
      cpg.call.code.l should contain("canThrow()")
    }

  }

}
