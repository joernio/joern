// This test file has been translated from swift/test/Parse/enum_element_pattern_swift4.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EnumElementPatternSwift4Tests extends AbstractPassTest {

  "EnumElementPatternSwift4Tests" should {

    "testEnumElementPatternSwift42" ignore AstFixture("""
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
        |""".stripMargin) { cpg => ??? }

    "testEnumElementPatternSwift43" ignore AstFixture("""
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
        |""".stripMargin) { cpg => ??? }

    "testEnumElementPatternSwift44" ignore AstFixture("""
        |extension E : Error {}
        |      func canThrow() throws {
        |  throw E.A
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnumElementPatternSwift45" ignore AstFixture("""
        |do {
        |  try canThrow()
        |      } catch E.A() {
        |  // ..
        |      } catch E.B(let payload) {
        |  let _: () = payload
        |}
        |""".stripMargin) { cpg => ??? }

  }

}
