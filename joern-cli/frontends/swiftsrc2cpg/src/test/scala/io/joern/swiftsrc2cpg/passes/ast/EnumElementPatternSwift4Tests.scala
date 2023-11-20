

// This test file has been translated from swift/test/Parse/enum_element_pattern_swift4.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EnumElementPatternSwift4Tests extends AbstractPassTest {
  "testEnumElementPatternSwift41" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/46040
      // See test/Compatibility/enum_element_pattern.swift for Swift3 behavior.
      // As for FIXME cases: see https://github.com/apple/swift/issues/46054
      """
    )
  }

  "testEnumElementPatternSwift42" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E {
        case A, B, C, D
        static func testE(e: E) {
          switch e {
          case A<UndefinedTy>():
            break
          case B<Int>():
            break
          default:
            break;
          }
        }
      }
      """
    )
  }

  "testEnumElementPatternSwift43" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func testE(e: E) {
        switch e {
        case E.A<UndefinedTy>():
          break
        case E.B<Int>():
          break
        case .C():
          break
        case .D(let payload):
          let _: () = payload
          break
        default:
          break
        }
        guard
          case .C() = e,
          case .D(let payload) = e
        else { return }
      }
      """
    )
  }

  "testEnumElementPatternSwift44" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension E : Error {}
      func canThrow() throws {
        throw E.A
      }
      """
    )
  }

  "testEnumElementPatternSwift45" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        try canThrow()
      } catch E.A() {
        // ..
      } catch E.B(let payload) {
        let _: () = payload
      }
      """
    )
  }

}
