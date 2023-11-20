

// This test file has been translated from swift/test/Parse/super.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SuperTests extends AbstractPassTest {
  "testSuper1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class B {
        var foo: Int
        func bar() {}
        init() {}
        init(x: Int) {}
        subscript(x: Int) -> Int {
          get {}
          set {}
        }
      }
      """
    )
  }

  "testSuper2a" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        override init() {
          super.init()
          super.init(42)
        }
      }
      """#
    )
  }

  "testSuper2b" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        override init(x:Int) {
          let _: () -> B = super.init
        }
      }
      """#
    )
  }

  "testSuper2c" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        convenience init(y:Int) {
          let _: () -> D = self.init
        }
      }
      """#
    )
  }

  "testSuper2d" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        init(z: Int) {
          super
            .init(x: z)
        }
      }
      """#
    )
  }

  "testSuper2e" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        func super_calls() {
          super.foo
          super.foo.bar
          super.bar
          super.bar()
          // FIXME: should also say "'super.init' cannot be referenced outside of an initializer"
          super.init
          super.init()
          super.init(0)
          super[0]
          super
            .bar()
        }
      }
      """#
    )
  }

  "testSuper2f" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        func bad_super_1() {
          super.1️⃣$0
        }
      }
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected name in member access", fixIts: ["insert name"])
      ],
      fixedSource: #"""
        class D : B {
          func bad_super_1() {
            super.<#identifier#>$0
          }
        }
        """#
    )
  }

  "testSuper2g" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        func bad_super_2() {
          super(0)
        }
      }
      """#,
      substructure: FunctionCallExprSyntax(
        calledExpression: SuperExprSyntax(superKeyword: .keyword(.super)),
        leftParen: .leftParenToken(),
        arguments: LabeledExprListSyntax([
          LabeledExprSyntax(
            expression: IntegerLiteralExprSyntax(literal: .integerLiteral("0"))
          )
        ]),
        rightParen: .rightParenToken()
      )
    )
  }

  "testSuper2h" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class D : B {
        func bad_super_3() {
          super
            [1]
        }
      }
      """#,
      substructure: ArrayExprSyntax(
        leftSquare: .leftSquareToken(),
        elements: ArrayElementListSyntax([
          ArrayElementSyntax(
            expression: IntegerLiteralExprSyntax(literal: .integerLiteral("1"))
          )
        ]),
        rightSquare: .rightSquareToken()
      )
    )
  }

  "testSuper3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class Closures : B {
        func captureWeak() {
          let g = { [weak self] () -> Void in // expected-note * {{'self' explicitly captured here}}
            super.foo()
          }
          g()
        }
        func captureUnowned() {
          let g = { [unowned self] () -> Void in // expected-note * {{'self' explicitly captured here}}
            super.foo()
          }
          g()
        }
        func nestedInner() {
          let g = { () -> Void in
            let h = { [weak self] () -> Void in // expected-note * {{'self' explicitly captured here}}
              super.foo()
              nil ?? super.foo()
            }
            h()
          }
          g()
        }
        func nestedOuter() {
          let g = { [weak self] () -> Void in // expected-note * {{'self' explicitly captured here}}
            let h = { () -> Void in
              super.foo()
              nil ?? super.foo()
            }
            h()
          }
          g()
        }
      }
      """
    )
  }

}
