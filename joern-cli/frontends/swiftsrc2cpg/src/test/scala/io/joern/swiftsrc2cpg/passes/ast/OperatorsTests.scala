

// This test file has been translated from swift/test/Parse/operators.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class OperatorsTests extends AbstractPassTest {
  "testOperators1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // This disables importing the stdlib intentionally.
      """
    )
  }

  "testOperators2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator == : Equal
      precedencegroup Equal {
        associativity: left
        higherThan: FatArrow
      }
      """
    )
  }

  "testOperators3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator & : BitAnd
      precedencegroup BitAnd {
        associativity: left
        higherThan: Equal
      }
      """
    )
  }

  "testOperators4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator => : FatArrow
      precedencegroup FatArrow {
        associativity: right
        higherThan: AssignmentPrecedence
      }
      precedencegroup AssignmentPrecedence {
        assignment: true
      }
      """
    )
  }

  "testOperators5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup DefaultPrecedence {}
      """
    )
  }

  "testOperators6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Man {}
      struct TheDevil {}
      struct God {}
      """
    )
  }

  "testOperators7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Five {}
      struct Six {}
      struct Seven {}
      """
    )
  }

  "testOperators8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct ManIsFive {}
      struct TheDevilIsSix {}
      struct GodIsSeven {}
      """
    )
  }

  "testOperators9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct TheDevilIsSixThenGodIsSeven {}
      """
    )
  }

  "testOperators10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func == (x: Man, y: Five) -> ManIsFive {}
      func == (x: TheDevil, y: Six) -> TheDevilIsSix {}
      func == (x: God, y: Seven) -> GodIsSeven {}
      """
    )
  }

  "testOperators11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func => (x: TheDevilIsSix, y: GodIsSeven) -> TheDevilIsSixThenGodIsSeven {}
      func => (x: ManIsFive, y: TheDevilIsSixThenGodIsSeven) {}
      """
    )
  }

  "testOperators12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "test1" ignore AstFixture("") { cpg =>
        Man() == Five() => TheDevil() == Six() => God() == Seven()
      }
      """
    )
  }

  "testOperators13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix operator *!*
      prefix operator *!*
      """
    )
  }

  "testOperators14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct LOOK {}
      struct LOOKBang {
        func exclaim() {}
      }
      """
    )
  }

  "testOperators15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix func *!* (x: LOOK) -> LOOKBang {}
      prefix func *!* (x: LOOKBang) {}
      """
    )
  }

  "testOperators16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "test2" ignore AstFixture("") { cpg =>
        *!*LOOK()*!*
      }
      """
    )
  }

  "testOperators17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // This should be parsed as (x*!*).exclaim()
      LOOK()*!*.exclaim()
      """
    )
  }

  "testOperators18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator ^
      infix operator ^
      postfix operator ^
      """
    )
  }

  "testOperators19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix func ^ (x: God) -> TheDevil {}
      prefix func ^ (x: TheDevil) -> God {}
      """
    )
  }

  "testOperators20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func ^ (x: TheDevil, y: God) -> Man {}
      """
    )
  }

  "testOperators21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var _ : TheDevil = God()^
      var _ : God = ^TheDevil()
      var _ : Man = TheDevil() ^ God()
      var _ : Man = God()^ ^ ^TheDevil()
      let _ = God()^TheDevil()
      """
    )
  }

  "testOperators22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix func ^ (x: Man) -> () -> God {
        return { return God() }
      }
      """
    )
  }

  "testOperators23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var _ : God = Man()^()
      """
    )
  }

  "testOperators24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func &(x : Man, y : Man) -> Man { return x } // forgive amp_prefix token
      """
    )
  }

  "testOperators25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator ⚽️
      """
    )
  }

  "testOperators26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix func ⚽️(x: Man) { }
      """
    )
  }

  "testOperators27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator ?? : OptTest
      precedencegroup OptTest {
        associativity: right
      }
      """
    )
  }

  "testOperators28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func ??(x: Man, y: TheDevil) -> TheDevil {
        return y
      }
      """
    )
  }

  "testOperators29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func test3(a: Man, b: Man, c: TheDevil) -> TheDevil {
        return a ?? b ?? c
      }
      """
    )
  }

  "testOperators30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // <rdar://problem/17821399> We don't parse infix operators bound on both
      // sides that begin with ! or ? correctly yet.
      infix operator !!
      """
    )
  }

  "testOperators31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func !!(x: Man, y: Man) {}
      """
    )
  }

  "testOperators32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let foo = Man()
      """
    )
  }

  "testOperators33" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let bar = TheDevil()
      """
    )
  }

  "testOperators34a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo!!1️⃣foo
      """,
      diagnostics: [
        DiagnosticSpec(message: "consecutive statements on a line must be separated by newline or ';'", fixIts: ["insert newline", "insert ';'"])
      ],
      applyFixIts: ["insert newline"],
      fixedSource: """
        foo!!
        foo
        """
    )
  }

  "testOperators34b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo!!1️⃣foo
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        )
      ],
      applyFixIts: ["insert ';'"],
      fixedSource: """
        foo!!; foo
        """
    )
  }

  "testOperators35a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo??1️⃣bar
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        )
      ],
      applyFixIts: ["insert newline"],
      fixedSource: """
        foo??
        bar
        """
    )
  }

  "testOperators35b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo??1️⃣bar
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        )
      ],
      applyFixIts: ["insert ';'"],
      fixedSource: """
        foo??; bar
        """
    )
  }
}
