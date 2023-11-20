

// This test file has been translated from swift/test/Parse/switch.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SwitchTests extends AbstractPassTest {
  "testSwitch1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func ~= (x: (Int,Int), y: (Int,Int)) -> Bool {
        return true
      }
      """
    )
  }

  "testSwitch2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func parseError1(x: Int) {
        switch 1️⃣func 2️⃣{}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected expression and '{}' to end 'switch' statement", fixIts: ["insert expression and '{}'"]),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected identifier and function signature in function",
          fixIts: ["insert identifier and function signature"]
        ),
      ],
      fixedSource: """
        func parseError1(x: Int) {
          switch <#expression#> {
        }func <#identifier#>() {}
        }
        """
    )
  }

  "testSwitch3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func parseError2(x: Int) {
        switch x 1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '{}' in 'switch' statement", fixIts: ["insert '{}'"])
      ],
      fixedSource: """
        func parseError2(x: Int) {
          switch x {
        }
        }
        """
    )
  }

  "testSwitch4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func parseError3(x: Int) {
        switch x {
          case 1️⃣
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression and ':' in switch case", fixIts: ["insert expression and ':'"])
      ],
      fixedSource: """
        func parseError3(x: Int) {
          switch x {
            case <#expression#>:
          }
        }
        """
    )
  }

  "testSwitch5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func parseError4(x: Int) {
        switch x {
        case var z where 1️⃣
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression in 'where' clause", fixIts: ["insert expression"]),
        DiagnosticSpec(message: "expected ':' in switch case", fixIts: ["insert ':'"]),
      ],
      fixedSource: """
        func parseError4(x: Int) {
          switch x {
          case var z where <#expression#>:
          }
        }
        """
    )
  }

  "testSwitch6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func parseError5(x: Int) {
        switch x {
        case let z 1️⃣
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in switch case", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        func parseError5(x: Int) {
          switch x {
          case let z:
          }
        }
        """
    )
  }

  "testSwitch7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func parseError6(x: Int) {
        switch x {
        default 1️⃣
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in switch case", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        func parseError6(x: Int) {
          switch x {
          default:
          }
        }
        """
    )
  }

  "testSwitch8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var x: Int
      """
    )
  }

  "testSwitch9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {}
      """
    )
  }

  "testSwitch10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        x = 0
      // Multiple patterns per case
      case 1, 2, 3:
        x = 0
      // 'where' guard
      case _ where x % 2 == 0:
        x = 1
        x = 2
        x = 3
      case _ where x % 2 == 0,
           _ where x % 3 == 0:
        x = 1
      case 10,
           _ where x % 3 == 0:
        x = 1
      case _ where x % 2 == 0,
           20:
        x = 1
      case var y where y % 2 == 0:
        x = y + 1
      case _ where 0:
        x = 0
      default:
        x = 1
      }
      """
    )
  }

  "testSwitch11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Multiple cases per case block
      switch x {
      case 0:
      case 1:
        x = 0
      }
      """
    )
  }

  "testSwitch12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
      default:
        x = 0
      }
      """
    )
  }

  "testSwitch13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        x = 0
      case 1:
      }
      """
    )
  }

  "testSwitch14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        x = 0
      default:
      }
      """
    )
  }

  "testSwitch15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        1️⃣;
      case 1:
        x = 0
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "standalone ';' statements are not allowed", fixIts: ["remove ';'"])
      ],
      fixedSource: """
        switch x {
        case 0:

        case 1:
          x = 0
        }
        """
    )
  }

  "testSwitch16a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
        1️⃣x = 1
      default:
        x = 0
      case 0:
        x = 0
      case 1:
        x = 0
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "all statements inside a switch must be covered by a 'case' or 'default' label", fixIts: ["insert label"])
      ],
      fixedSource: """
        switch x {
        case <#identifier#>:
          x = 1
        default:
          x = 0
        case 0:
          x = 0
        case 1:
          x = 0
        }
        """
    )
  }

  "testSwitch16b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
        1️⃣let x = 1
      case 1:
        x = 0
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "all statements inside a switch must be covered by a 'case' or 'default' label", fixIts: ["insert label"])
      ],
      fixedSource: """
        switch x {
        case <#identifier#>:
          let x = 1
        case 1:
          x = 0
        }
        """
    )
  }

  "testSwitch17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      default:
        x = 0
      default:
        x = 0
      }
      """
    )
  }

  "testSwitch18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
        1️⃣x = 1
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "all statements inside a switch must be covered by a 'case' or 'default' label", fixIts: ["insert label"])
      ],
      fixedSource: """
        switch x {
        case <#identifier#>:
          x = 1
        }
        """
    )
  }

  "testSwitch19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
        1️⃣x = 1
        x = 2
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "all statements inside a switch must be covered by a 'case' or 'default' label", fixIts: ["insert label"])
      ],
      fixedSource: """
        switch x {
        case <#identifier#>:
          x = 1
          x = 2
        }
        """
    )
  }

  "testSwitch20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      default:
      case 0:
        x = 0
      }
      """
    )
  }

  "testSwitch21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      default:
      default:
        x = 0
      }
      """
    )
  }

  "testSwitch22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      default 1️⃣where x == 0:
        x = 0
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'default' cannot be used with a 'where' guard expression")
      ]
    )
  }

  "testSwitch23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
      }
      """
    )
  }

  "testSwitch24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
      case 1:
        x = 0
      }
      """
    )
  }

  "testSwitch25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        x = 0
      case 1:
      }
      """
    )
  }

  "testSwitch26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣case 0:
      var y = 0
      2️⃣default:
      var z = 1
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'case' can only appear inside a 'switch' statement or 'enum' declaration"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "'default' label can only appear inside a 'switch' statement"),
      ]
    )
  }

  "testSwitch27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      fallthrough
      """
    )
  }

  "testSwitch28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        fallthrough
      case 1:
        fallthrough
      default:
        fallthrough
      }
      """
    )
  }

  "testSwitch29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Fallthrough can transfer control anywhere within a case and can appear
      // multiple times in the same case.
      switch x {
      case 0:
        if true { fallthrough }
        if false { fallthrough }
        x += 1
      default:
        x += 1
      }
      """
    )
  }

  "testSwitch30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Cases cannot contain 'var' bindings if there are multiple matching patterns
      // attached to a block. They may however contain other non-binding patterns.
      """
    )
  }

  "testSwitch31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var t = (1, 2)
      """
    )
  }

  "testSwitch32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch t {
      case (var a, 2), (1, _):
        ()
      case (_, 2), (var a, _):
        ()
      case (var a, 2), (1, var b):
        ()
      case (var a, 2):
      case (1, _):
        ()
      case (_, 2):
      case (1, var a):
        ()
      case (var a, 2):
      case (1, var b):
        ()
      case (1, let b): // let bindings expected-warning {{immutable value 'b' was never used; consider replacing with '_' or removing it}}
        ()
      case (_, 2), (let a, _):
        ()
      // OK
      case (_, 2), (1, _):
        ()
      case (_, var a), (_, var a):
        ()
      case (var a, var b), (var b, var a):
        ()
      case (_, 2):
      case (1, _):
        ()
      }
      """
    )
  }

  "testSwitch33" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func patternVarUsedInAnotherPattern(x: Int) {
        switch x {
        case let a,
             value:
          break
        }
      }
      """
    )
  }

  "testSwitch34" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Fallthroughs can only transfer control into a case label with bindings if the previous case binds a superset of those vars.
      switch t {
      case (1, 2):
        fallthrough
      case (var a, var b):
        t = (b, a)
      }
      """
    )
  }

  "testSwitch35" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch t { // specifically notice on next line that we shouldn't complain that a is unused - just never mutated
      case (var a, let b):
        t = (b, b)
        fallthrough // ok - notice that subset of bound variables falling through is fine
      case (2, let a):
        t = (a, a)
      }
      """
    )
  }

  "testSwitch36" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func patternVarDiffType(x: Int, y: Double) {
        switch (x, y) {
        case (1, let a):
          fallthrough
        case (let a, _):
          break
        }
      }
      """
    )
  }

  "testSwitch37" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func patternVarDiffMutability(x: Int, y: Double) {
        switch x {
        case let a where a < 5, var a where a > 10:
          break
        default:
          break
        }
        switch (x, y) {
        // Would be nice to have a fixit in the following line if we detect that all bindings in the same pattern have the same problem.
        case let (a, b) where a < 5, var (a, b) where a > 10: // expected-error 2{{'var' pattern binding must match previous 'let' pattern binding}}{{none}}
          break
        case (let a, var b) where a < 5, (let a, let b) where a > 10:
          break
        case (let a, let b) where a < 5, (var a, let b) where a > 10, (let a, var b) where a == 8:
          break
        default:
          break
        }
      }
      """
    )
  }

  "testSwitch38" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func test_label(x : Int) {
      Gronk:
        switch x {
        case 42: return
        }
      }
      """
    )
  }

  "testSwitch39" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func enumElementSyntaxOnTuple() {
        switch (1, 1) {
        case .Bar:
          break
        default:
          break
        }
      }
      """
    )
  }

  "testSwitch40" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/42798
      enum Whatever { case Thing }
      func f0(values: [Whatever]) {
          switch value {
          case .Thing: // Ok. Don't emit diagnostics about enum case not found in type <<error type>>.
              break
          }
      }
      """
    )
  }

  "testSwitch41" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // https://github.com/apple/swift/issues/43334
      // https://github.com/apple/swift/issues/43335
      enum Whichever {
        case Thing
        static let title = "title"
        static let alias: Whichever = .Thing
      }
      func f1(x: String, y: Whichever) {
        switch x {
          case Whichever.title: // Ok. Don't emit diagnostics for static member of enum.
              break
          case Whichever.buzz:
              break
          case Whichever.alias:
          default:
            break
        }
        switch y {
          case Whichever.Thing: // Ok.
              break
          case Whichever.alias: // Ok. Don't emit diagnostics for static member of enum.
              break
          case Whichever.title:
              break
        }
        switch y {
          case .alias:
            break
          default:
            break
        }
      }
      """#
    )
  }

  "testSwitch42" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
      @unknown case _:
        x = 0
      }
      """
    )
  }

  "testSwitch43" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
      @unknown default:
        x = 0
      }
      """
    )
  }

  "testSwitch44" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        x = 0
      @unknown case _:
      }
      """
    )
  }

  "testSwitch45" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        x = 0
      @unknown default:
      }
      """
    )
  }

  "testSwitch46" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown default:
        x = 0
      default:
        x = 0
      case .Thing:
        x = 0
      }
      """
    )
  }

  "testSwitch47" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      default:
        x = 0
      @unknown case _:
        x = 0
      case .Thing:
        x = 0
      }
      """
    )
  }

  "testSwitch48" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      default:
        x = 0
      @unknown default:
        x = 0
      case .Thing:
        x = 0
      }
      """
    )
  }

  "testSwitch49" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown default 1️⃣where x == 0:
        x = 0
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'default' cannot be used with a 'where' guard expression")
      ]
    )
  }

  "testSwitch50" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _:
        fallthrough
      }
      """
    )
  }

  "testSwitch51" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _:
        fallthrough
      case .Thing:
        break
      }
      """
    )
  }

  "testSwitch52" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown default:
        fallthrough
      case .Thing:
        break
      }
      """
    )
  }

  "testSwitch53" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _, _:
        break
      }
      """
    )
  }

  "testSwitch54" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _, _, _:
        break
      }
      """
    )
  }

  "testSwitch55" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case let value:
        _ = value
      }
      """
    )
  }

  "testSwitch56" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch (Whatever.Thing, Whatever.Thing) {
      @unknown case (_, _):
        break
      }
      """
    )
  }

  "testSwitch57" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case is Whatever:
        break
      }
      """
    )
  }

  "testSwitch58" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case .Thing:
        break
      }
      """
    )
  }

  "testSwitch59" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case (_): // okay
        break
      }
      """
    )
  }

  "testSwitch60" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _ where x == 0:
        break
      }
      """
    )
  }

  "testSwitch61" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown default 1️⃣where x == 0:
        break
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'default' cannot be used with a 'where' guard expression")
      ]
    )
  }

  "testSwitch62" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        x = 0
      #if true
      @unknown case _:
        x = 0
      #endif
      }
      """
    )
  }

  "testSwitch63" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        break
      @garbage case _:
        break
      }
      """
    )
  }

  "testSwitch64" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 0:
        break
      @garbage 1️⃣@moreGarbage default:
        break
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '@moreGarbage' in switch case")
      ]
    )
  }

  "testSwitch65" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @unknown let _ = 1
      """
    )
  }

  "testSwitch66" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case _:
        @unknown let _ = 1
      }
      """
    )
  }

  "testSwitch67" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        break
      @unknown1️⃣(garbage) case _:
        break
      }
      switch Whatever.Thing {
      case .Thing:
        break
      @unknown
      2️⃣@unknown
      case _:
        break
      }
      switch Whatever.Thing {
      @unknown 3️⃣@garbage(foobar)
      case _:
        break
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code '(garbage)' in switch case"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '@unknown' in switch case"),
        DiagnosticSpec(locationMarker: "3️⃣", message: "unexpected code '@garbage(foobar)' in switch case"),
      ]
    )
  }

  "testSwitch68" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case 1:
        break
      @unknown case _:
        break
      }
      """
    )
  }

  "testSwitch69" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      @unknown case _:
        break
      }
      """
    )
  }

  "testSwitch70" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch71" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        break
      @unknown case _:
        break
      @unknown case _:
        break
      }
      """
    )
  }

  "testSwitch72" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        break
      @unknown case _:
        break
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch73" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      case .Thing:
        break
      @unknown default:
        break
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch74" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _:
        break
      @unknown case _:
        break
      }
      """
    )
  }

  "testSwitch75" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown case _:
        break
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch76" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch Whatever.Thing {
      @unknown default:
        break
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch77" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      @unknown case _:
        break
      @unknown case _:
        break
      }
      """
    )
  }

  "testSwitch78" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      @unknown case _:
        break
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch79" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      @unknown default:
        break
      @unknown default:
        break
      }
      """
    )
  }

  "testSwitch80" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testReturnBeforeUnknownDefault" ignore AstFixture("") { cpg =>
        switch x {
        case 1:
          return
        @unknown default:
          break
        }
      }
      """
    )
  }

  "testSwitch81" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testReturnBeforeIncompleteUnknownDefault" ignore AstFixture("") { cpg =>
        switch x {
        case 1:
          return
        @unknown default 1️⃣
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in switch case", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        "testReturnBeforeIncompleteUnknownDefault" ignore AstFixture("") { cpg =>
          switch x {
          case 1:
            return
          @unknown default:
          }
        }
        """
    )
  }

  "testSwitch82" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testReturnBeforeIncompleteUnknownDefault2" ignore AstFixture("") { cpg =>
        switch x {
        case 1:
          return
        @unknown 1️⃣
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected label in switch case", fixIts: ["insert label"])
      ],
      fixedSource: """
        "testReturnBeforeIncompleteUnknownDefault2" ignore AstFixture("") { cpg =>
          switch x {
          case 1:
            return
          @unknown case <#identifier#>:
          }
        }
        """
    )
  }

  "testSwitch83" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testIncompleteArrayLiteral" ignore AstFixture("") { cpg =>
        switch x {
        case 1:
          _ = ℹ️[1 1️⃣
        @unknown default:
          ()
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ']' to end array",
          notes: [
            NoteSpec(message: "to match this opening '['")
          ],
          fixIts: ["insert ']'"]
        )
      ],
      fixedSource: """
        "testIncompleteArrayLiteral" ignore AstFixture("") { cpg =>
          switch x {
          case 1:
            _ = [1]
          @unknown default:
            ()
          }
        }
        """
    )
  }
}
