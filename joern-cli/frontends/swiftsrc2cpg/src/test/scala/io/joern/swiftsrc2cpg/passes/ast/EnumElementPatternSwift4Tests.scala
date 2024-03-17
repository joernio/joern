// This test file has been translated from swift/test/Parse/enum_element_pattern_swift4.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class EnumElementPatternSwift4Tests extends AstSwiftSrc2CpgSuite {

  "EnumElementPatternSwift4Tests" should {

    "testEnumElementPatternSwift42" ignore {
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
      ???
    }

    "testEnumElementPatternSwift43" ignore {
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
      ???
    }

    "testEnumElementPatternSwift44" ignore {
      val cpg = code("""
        |extension E : Error {}
        |      func canThrow() throws {
        |  throw E.A
        |}
        |""".stripMargin)
      ???
    }

    "testEnumElementPatternSwift45" ignore {
      val cpg = code("""
        |do {
        |  try canThrow()
        |      } catch E.A() {
        |  // ..
        |      } catch E.B(let payload) {
        |  let _: () = payload
        |}
        |""".stripMargin)
      ???
    }

  }

}
