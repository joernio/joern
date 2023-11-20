package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class VariadicGenericsTests extends AbstractPassTest {
  "testSimpleForwarding" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func tuplify<each T>(_ t: repeat each T) -> (repeat each T) {
        return (1️⃣repeat each t)
      }
      """,
      substructure: PackExpansionExprSyntax(
        repeatKeyword: .keyword(.repeat),
        repetitionPattern: PackElementExprSyntax(
          eachKeyword: .keyword(.each),
          pack: DeclReferenceExprSyntax(
            baseName: .identifier("t")
          )
        )
      ),
      substructureAfterMarker: "1️⃣"
    )
  }

  "testRequirement" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func requirement<each T>() where each T: P {
      }
      """
    )
  }

  "testElementOutsideExpansion" ignore AstFixture("") { cpg =>
    // This is valid to parse, and becomes invalid during type resolution.
    assertParse(
      """
      func invalid<each T>(_ t: each T) -> each T {}
      """
    )
  }

  "testInvalidPackElement" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func invalid<each T>() -> (each any 1️⃣T) {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ',' in tuple type", fixIts: ["insert ','"])
      ],
      fixedSource: """
        func invalid<each T>() -> (each any, T) {}
        """
    )

    assertParse(
      """
      func invalid<each T>(_: each T 1️⃣& P) {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '& P' in parameter clause")
      ]
    )
  }

  "testTypeParameterPackEllipsis" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func invalid<T1️⃣...>(_ t: repeat each T) {}
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "ellipsis operator cannot be used with a type parameter pack",
          fixIts: ["replace '...' with 'each'"]
        )
      ],
      fixedSource: """
        func invalid<each T>(_ t: repeat each T) {}
        """
    )
  }

  "testTypeParameterPackEachEllipsis" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func invalid<each T1️⃣...>(_ t: repeat each T) {}
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "ellipsis operator cannot be used with a type parameter pack",
          fixIts: ["remove '...'"]
        )
      ],
      fixedSource: """
        func invalid<each T>(_ t: repeat each T) {}
        """
    )
  }

  "testPackElementExprSimple" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func tuplify<each T>(_ t: repeat each T) -> (repeat each T) {
        return (repeat each t)
      }
      """
    )

    assertParse(
      """
      func zip<each T, each U>(_ first: repeat each T, with second: repeat each U) -> (repeat (each T, each U)) {
        return (repeat (each first, each second))
      }
      """
    )

    assertParse(
      """
      func variadicMap<each T, each Result>(_ t: repeat each T, transform: repeat (each T) -> each Result) -> (repeat each Result) {
        return (repeat (each transform)(each t))
      }
      """
    )
  }

  "testEachExprKeyword" ignore AstFixture("") { cpg =>
    let callExpr = FunctionCallExprSyntax(
      calledExpression: DeclReferenceExprSyntax(
        baseName: .identifier("each")
      ),
      leftParen: .leftParenToken(),
      arguments: LabeledExprListSyntax([
        .init(
          expression:
            DeclReferenceExprSyntax(
              baseName: .identifier("x")
            )
        )
      ]),
      rightParen: .rightParenToken()
    )

    assertParse(
      """
      func test() {
      1️⃣each(x)
      }
      """,
      substructure: callExpr,
      substructureAfterMarker: "1️⃣"
    )

    assertParse(
      """
      func test() {
      1️⃣each (x)
      }
      """,
      substructure: callExpr,
      substructureAfterMarker: "1️⃣"
    )

    assertParse(
      """
      func test() {
      1️⃣each x
      }
      """,
      substructure: PackElementExprSyntax(
        eachKeyword: .keyword(.each),
        pack: DeclReferenceExprSyntax(
          baseName: .identifier("x")
        )
      ),
      substructureAfterMarker: "1️⃣"
    )
  }

  "testPackExpansionExpr" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func expand<each T>(_ t: repeat each T) {
        1️⃣repeat (each t).member()
      }
      """,
      substructure: PackExpansionExprSyntax(
        repeatKeyword: .keyword(.repeat),
        repetitionPattern: FunctionCallExprSyntax(
          callee: MemberAccessExprSyntax(
            base: ExprSyntax(
              TupleExprSyntax(
                elements: .init([
                  LabeledExprSyntax(
                    expression: PackElementExprSyntax(
                      eachKeyword: .keyword(.each),
                      pack: DeclReferenceExprSyntax(
                        baseName: .identifier("t")
                      )
                    )
                  )
                ])
              )
            ),
            name: "member"
          )
        )
      ),
      substructureAfterMarker: "1️⃣"
    )

    assertParse(
      """
      func expand<each T>(_ t: repeat each T) {
        1️⃣repeat each t.member
      }
      """,
      substructure: PackExpansionExprSyntax(
        repeatKeyword: .keyword(.repeat),
        repetitionPattern: PackElementExprSyntax(
          eachKeyword: .keyword(.each),
          pack: MemberAccessExprSyntax(
            base: ExprSyntax(
              DeclReferenceExprSyntax(
                baseName: .identifier("t")
              )
            ),
            name: "member"
          )
        )
      ),
      substructureAfterMarker: "1️⃣"
    )

    assertParse(
      """
      func expand<each T>(_ t: repeat each T) {
        1️⃣repeat x + each t + 10
      }
      """,
      substructure: PackExpansionExprSyntax(
        repeatKeyword: .keyword(.repeat),
        repetitionPattern: SequenceExprSyntax(
          elements: .init([
            ExprSyntax(
              DeclReferenceExprSyntax(
                baseName: .identifier("x")
              )
            ),
            ExprSyntax(
              BinaryOperatorExprSyntax(
                operator: .binaryOperator("+")
              )
            ),
            ExprSyntax(
              PackElementExprSyntax(
                eachKeyword: .keyword(.each),
                pack: DeclReferenceExprSyntax(
                  baseName: .identifier("t")
                )
              )
            ),
            ExprSyntax(
              BinaryOperatorExprSyntax(
                operator: .binaryOperator("+")
              )
            ),
            ExprSyntax(
              IntegerLiteralExprSyntax(
                integerLiteral: 10
              )
            ),
          ])
        )
      ),
      substructureAfterMarker: "1️⃣"
    )
  }

  "testMetatype" ignore AstFixture("") { cpg =>
    assertParse(
      """
      G<Int, repeat Array<each T>>.self
      """
    )
  }
}

class TypeParameterPackTests extends AbstractPassTest {
  "testParameterPacks1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func f1<each T>() -> repeat each T {}
      """
    )
  }
  "testParameterPacks2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func f2<each T>() -> (repeat each T) {}
      """
    )
  }
  "testParameterPacks3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func f3<each T>() -> G<repeat each T> {}
      """
    )
  }
  "testParameterPacks4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol P {
        associatedtype 1️⃣each T
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "associated types cannot be variadic",
          fixIts: ["remove 'each'"]
        )
      ],
      fixedSource: """
        protocol P {
          associatedtype T
        }
        """
    )
  }
  "testParameterPacks4EarlySyntax" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol P {
        associatedtype T1️⃣...
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "associated types cannot be variadic",
          fixIts: ["remove '...'"]
        )
      ],
      fixedSource: """
        protocol P {
          associatedtype T
        }
        """
    )
  }
  "testParameterPacks5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      typealias Alias<each T> = (repeat each T)
      """
    )
  }
  "testParameterPacks6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<each T> {}
      """
    )
  }
  "testParameterPacks7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<T, each U> {}
      """
    )
  }
  "testParameterPacks8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<each T, U> {}
      """
    )
  }
  "testParameterPacks9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<each T:P, U> {}
      """
    )
  }
  "testParameterPacks10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<each T :P, U> {}
      """
    )
  }
  "testParameterPacks11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<each T: P> {}
      """
    )
  }
  "testParameterPacks12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S<each T : P> {}
      """
    )
  }
  "testParameterPacks13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<each T>(_ x: repeat each T) {}
      """
    )
  }
  "testParameterPacks14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<each T:P>(_ x: repeat each T) {}
      """
    )
  }
  "testParameterPacks15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<each T :P>(_ x: repeat each T) {}
      """
    )
  }
  "testParameterPacks16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<each T : P>(_ x: repeat each T) {}
      """
    )
  }
  "testParameterPacks17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<each T: P>(_ x: repeat each T) {}
      """
    )
  }
  "testParameterPacks18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<T, U, each V>(x: T, y: U, z: repeat each V) { }
      """
    )
  }
  "testParameterPacks19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<T, each U, V>(x: T, y: repeat each U, z: V) { }
      """
    )
  }
  "testParameterPacks20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<each T, each U, each V>(x: repeat each T, y: repeat each U, z: repeat each V) { }
      """
    )
  }
  "testParameterPacks21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        case f1(_: repeat each T)
      }
      """
    )
  }
  "testParameterPacks22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        case f2(_: G<repeat each T>)
      }
      """
    )
  }
  "testParameterPacks23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        var x: repeat each T { fatalError() }
      }
      """
    )
  }
  "testParameterPacks24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        var x: (repeat each T) { fatalError() }
      }
      """
    )
  }
  "testParameterPacks25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        subscript(_: repeat each T) -> Int { fatalError() }
      }
      """
    )
  }
  "testParameterPacks26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        subscript() -> repeat each T { fatalError() }
      }
      """
    )
  }
  "testParameterPacks27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E<each T> {
        subscript() -> (repeat each T) { fatalError() }
      }
      """
    )
  }

  "testVariadicTypes" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: G< > = G()
      let _: G<repeat each T> = G()
      let _: G<Int, repeat each T> = G()
      let _ = G< >.self
      let _ = G<repeat each T>.self
      let _ = G<Int, repeat each T>.self
      """
    )

  }

  func testMissingCommaInType() throws {
    assertParse(
      """
      var foo: (Int)
      """
    )

    assertParse(
      """
      var foo: (Int, Int)
      """
    )

    assertParse(
      """
      var foo: (bar: Int 1️⃣bar2: Int)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ',' in tuple type", fixIts: ["insert ','"])
      ],
      fixedSource: """
        var foo: (bar: Int, bar2: Int)
        """
    )

    assertParse(
      """
      var foo: (bar: Int 1️⃣Int)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ',' in tuple type", fixIts: ["insert ','"])
      ],
      fixedSource: """
        var foo: (bar: Int, Int)
        """
    )

    assertParse(
      """
      var foo: (a 1️⃣Int)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in tuple type", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        var foo: (a: Int)
        """
    )

    assertParse(
      """
      var foo: (A 1️⃣Int)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ',' in tuple type", fixIts: ["insert ','"])
      ],
      fixedSource: """
        var foo: (A, Int)
        """
    )

    assertParse(
      """
      var foo: (_ 1️⃣a 2️⃣Int)
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected ':' in tuple type", fixIts: ["insert ':'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected ',' in tuple type", fixIts: ["insert ','"]),
      ],
      fixedSource: """
        var foo: (_: a, Int)
        """
    )

    assertParse(
      """
      var foo: (Array<Foo> 1️⃣Array<Bar>)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ',' in tuple type", fixIts: ["insert ','"])
      ],
      fixedSource: """
        var foo: (Array<Foo>, Array<Bar>)
        """
    )

    assertParse(
      """
      var foo: (a 1️⃣Array<Bar>)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in tuple type", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        var foo: (a: Array<Bar>)
        """
    )

    assertParse(
      """
      var foo: (Array<Foo> 1️⃣a)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ',' in tuple type", fixIts: ["insert ','"])
      ],
      fixedSource: """
        var foo: (Array<Foo>, a)
        """
    )
  }
}
