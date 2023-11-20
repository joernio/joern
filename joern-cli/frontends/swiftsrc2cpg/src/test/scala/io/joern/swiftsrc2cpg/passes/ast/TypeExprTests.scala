

// This test file has been translated from swift/test/Parse/type_expr.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

// Types in expression contexts must be followed by a member access or
// constructor call.
class TypeExprTests extends AbstractPassTest {
  "testTypeExpr3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        struct Bar {
          init() {}
          static var prop: Int = 0
          static func meth() {}
          func instMeth() {}
        }
        init() {}
        static var prop: Int = 0
        static func meth() {}
        func instMeth() {}
      }
      """
    )
  }

  "testTypeExpr4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol Zim {
        associatedtype Zang
        init()
        // TODO class var prop: Int { get }
        static func meth() {}
        func instMeth() {}
      }
      """
    )
  }

  "testTypeExpr5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol Bad {
        init() {}
      }
      """
    )
  }

  "testTypeExpr6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Gen<T> {
        struct Bar {
          init() {}
          static var prop: Int { return 0 }
          static func meth() {}
          func instMeth() {}
        }
        init() {}
        static var prop: Int { return 0 }
        static func meth() {}
        func instMeth() {}
      }
      """
    )
  }

  "testTypeExpr7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func unqualifiedType() {
        _ = Foo.self
        _ = Foo.self
        _ = Foo()
        _ = Foo.prop
        _ = Foo.meth
        let _ : () = Foo.meth()
        _ = Foo.instMeth
        _ = Foo
        _ = Foo.dynamicType
        _ = Bad
      }
      """
    )
  }

  "testTypeExpr8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func qualifiedType() {
        _ = Foo.Bar.self
        let _ : Foo.Bar.Type = Foo.Bar.self
        let _ : Foo.Protocol = Foo.self
        _ = Foo.Bar()
        _ = Foo.Bar.prop
        _ = Foo.Bar.meth
        let _ : () = Foo.Bar.meth()
        _ = Foo.Bar.instMeth
        _ = Foo.Bar
        _ = Foo.Bar.dynamicType
      }
      """
    )

    assertParse("(X).Y.self", ExprSyntax.parse)
    assertParse("(X.Y).Z.self", ExprSyntax.parse)
    assertParse("((X).Y).Z.self", ExprSyntax.parse)
  }

  "testTypeExpr9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // We allow '.Type' in expr context
      func metaType() {
        let _ = Foo.Type.self
        let _ = Foo.Type.self
        let _ = Foo.Type
        let _ = type(of: Foo.Type)
      }
      """
    )
  }

  "testTypeExpr10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func genType() {
        _ = Gen<Foo>.self
        _ = Gen<Foo>()
        _ = Gen<Foo>.prop
        _ = Gen<Foo>.meth
        let _ : () = Gen<Foo>.meth()
        _ = Gen<Foo>.instMeth
        _ = Gen<Foo>

        _ = X?.self
        _ = [X].self
        _ = [X : Y].self
      }
      """
    )

    assertParse("X?.self", ExprSyntax.parse)
    assertParse("[X].self", ExprSyntax.parse)
    assertParse("[X : Y].self", ExprSyntax.parse)
  }

  "testTypeExpr11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func genQualifiedType() {
        _ = Gen<Foo>.Bar.self
        _ = Gen<Foo>.Bar()
        _ = Gen<Foo>.Bar.prop
        _ = Gen<Foo>.Bar.meth
        let _ : () = Gen<Foo>.Bar.meth()
        _ = Gen<Foo>.Bar.instMeth
        _ = Gen<Foo>.Bar
        _ = Gen<Foo>.Bar.dynamicType

        _ = (G<X>).Y.self
        _ = X?.Y.self
        _ = (X)?.Y.self
        _ = (X?).Y.self
        _ = [X].Y.self
        _ = [X : Y].Z.self
      }
      """
    )

    assertParse("(G<X>).Y.self", ExprSyntax.parse)
    assertParse("X?.Y.self", ExprSyntax.parse)
    assertParse("(X)?.Y.self", ExprSyntax.parse)
    assertParse("(X?).Y.self", ExprSyntax.parse)
    assertParse("[X].Y.self", ExprSyntax.parse)
    assertParse("[X : Y].Z.self", ExprSyntax.parse)
  }

  "testTypeExpr12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func typeOfShadowing() {
        // Try to shadow type(of:)
        func type<T>(of t: T.Type, flag: Bool) -> T.Type {
          return t
        }
        func type<T, U>(of t: T.Type, _ : U) -> T.Type {
          return t
        }
        func type<T>(_ t: T.Type) -> T.Type {
          return t
        }
        func type<T>(fo t: T.Type) -> T.Type {
          return t
        }
        _ = type(of: Gen<Foo>.Bar)
        _ = type(Gen<Foo>.Bar)
        _ = type(of: Gen<Foo>.Bar.self, flag: false) // No error here.
        _ = type(fo: Foo.Bar.self) // No error here.
        _ = type(of: Foo.Bar.self, [1, 2, 3]) // No error here.
      }
      """
    )
  }

  "testTypeExpr13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func archetype<T: Zim>(_: T) {
        _ = T.self
        _ = T()
        // TODO let prop = T.prop
        _ = T.meth
        let _ : () = T.meth()
        _ = T
      }
      """
    )
  }

  "testTypeExpr14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func assocType<T: Zim>(_: T) where T.Zang: Zim {
        _ = T.Zang.self
        _ = T.Zang()
        // TODO _ = T.Zang.prop
        _ = T.Zang.meth
        let _ : () = T.Zang.meth()
        _ = T.Zang
      }
      """
    )
  }

  "testTypeExpr15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class B {
        class func baseMethod() {}
      }
      class D: B {
        class func derivedMethod() {}
      }
      """
    )
  }

  "testTypeExpr16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func derivedType() {
        let _: B.Type = D.self
        _ = D.baseMethod
        let _ : () = D.baseMethod()
        let _: D.Type = D.self
        _ = D.derivedMethod
        let _ : () = D.derivedMethod()
        let _: B.Type = D
        let _: D.Type = D
      }
      """
    )
  }

  "testTypeExpr17" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // Referencing a nonexistent member or constructor should not trigger errors
      // about the type expression.
      func nonexistentMember() {
        let cons = Foo("this constructor does not exist")
        let prop = Foo.nonexistent
        let meth = Foo.nonexistent()
      }
      """#
    )
  }

  "testTypeExpr18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol P {}
      """
    )
  }

  "testTypeExpr19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func meta_metatypes() {
        let _: P.Protocol = P.self
        _ = P.Type.self
        _ = P.Protocol.self
        _ = P.Protocol.Protocol.self
        _ = P.Protocol.Type.self
        _ = B.Type.self
      }
      """
    )
  }

  "testTypeExpr20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class E {
        private init() {}
      }
      """
    )
  }

  "testTypeExpr21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func inAccessibleInit() {
        _ = E
      }
      """
    )
  }

  "testTypeExpr22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum F: Int {
        case A, B
      }
      """
    )
  }

  "testTypeExpr23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct G {
        var x: Int
      }
      """
    )
  }

  "testTypeExpr24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func implicitInit() {
        _ = F
        _ = G
      }
      """
    )
  }

  "testTypeExpr25a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [(Int) -> Int]()
      """
    )
  }

  "testTypeExpr25b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [(Int, Int) -> Int]()
      """
    )
  }

  "testTypeExpr25c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [(x: Int, y: Int) -> Int]()
      """
    )
  }

  "testTypeExpr25d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Make sure associativity is correct
      let a = [(Int) -> (Int) -> Int]()
      """
    )
  }

  "testTypeExpr25e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let b: Int = a[0](5)(4)
      """
    )
  }

  "testTypeExpr25f" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [String: (Int) -> Int]()
      """
    )
  }

  "testTypeExpr25g" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [String: (Int, Int) -> Int]()
      """
    )
  }

  "testTypeExpr25h" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [1 -> Int]()
      """
    )
  }

  "testTypeExpr25i" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [Int -> 1]()
      """
    )
  }

  "testTypeExpr25j" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Should parse () as void type when before or after arrow
      _ = [() -> Int]()
      """
    )
  }

  "testTypeExpr25k" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [(Int) -> ()]()
      """
    )
  }

  "testTypeExpr25l" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = 2 + () -> Int
      """
    )
  }

  "testTypeExpr25m" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = () -> (Int, Int).2
      """
    )
  }

  "testTypeExpr25n" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = (Int) -> Int
      """
    )
  }

  "testTypeExpr25o" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = @convention(c) () -> Int
      """
    )
  }

  "testTypeExpr25p" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = 1 + (@convention(c) () -> Int).self
      """
    )
  }

  "testTypeExpr25q" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = (@autoclosure () -> Int) -> (Int, Int).2
      """
    )
  }

  "testTypeExpr25r" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = ((@autoclosure () -> Int) -> (Int, Int)).1
      """
    )
  }

  "testTypeExpr25s" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = ((inout Int) -> Void).self
      """
    )
  }

  "testTypeExpr25t" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [(Int) throws -> Int]()
      """
    )
  }

  "testTypeExpr25u" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [@convention(swift) (Int) throws -> Int]().count
      """
    )
  }

  "testTypeExpr25v" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [(inout Int) throws -> (inout () -> Void) -> Void]().count
      """
    )
  }

  "testTypeExpr25w" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = [String: (@autoclosure (Int) -> Int32) -> Void]().keys
      """
    )
  }

  "testTypeExpr25x" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [(Int) -> 1️⃣throws Int]()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'throws' must precede '->'", fixIts: ["move 'throws' in front of '->'"])
      ],
      fixedSource: """
        let _ = [(Int) throws -> Int]()
        """
    )
  }

  "testTypeExpr25y" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [Int throws 1️⃣Int]();
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '->' in array element", fixIts: ["insert '->'"])
      ],
      fixedSource: """
        let _ = [Int throws -> Int]();
        """
    )
  }

  "testCompositionTypeExpr" ignore AstFixture("") { cpg =>
    assertParse("P & Q", ExprSyntax.parse)
    assertParse("P & Q.self", ExprSyntax.parse)
    assertParse("any P & Q", ExprSyntax.parse)

    assertParse("(P & Q).self", ExprSyntax.parse)
    assertParse("((P) & (Q)).self", ExprSyntax.parse)

    assertParse("(A.B & C.D).self", ExprSyntax.parse)
    assertParse("((A).B & (C).D).self", ExprSyntax.parse)

    assertParse("(G<X> & G<Y>).self", ExprSyntax.parse)
    assertParse("(X? & Y?).self", ExprSyntax.parse)
    assertParse("([X] & [Y]).self", ExprSyntax.parse)
    assertParse("([A : B] & [C : D]).self", ExprSyntax.parse)

    assertParse("(G<A>.B & G<C>.D).self", ExprSyntax.parse)
    assertParse("(A?.B & C?.D).self", ExprSyntax.parse)
    assertParse("([A].B & [A].B).self", ExprSyntax.parse)
    assertParse("([A : B].C & [D : E].F).self", ExprSyntax.parse)

    assertParse("(X.Type & Y.Type).self", ExprSyntax.parse)
    assertParse("(X.Protocol & Y.Protocol).self", ExprSyntax.parse)

    assertParse("((A, B) & (C, D)).self", ExprSyntax.parse)
  }

  "testTupleTypeExpr" ignore AstFixture("") { cpg =>
    assertParse("(X).self", ExprSyntax.parse)

    assertParse("(X, Y)", ExprSyntax.parse)

    assertParse("(X, Y).self", ExprSyntax.parse)
    assertParse("((X), (Y)).self", ExprSyntax.parse)

    assertParse("(A.B, C.D).self", ExprSyntax.parse)
    assertParse("((A).B, (C).D).self", ExprSyntax.parse)

    assertParse("(G<X>, G<Y>).self", ExprSyntax.parse)
    assertParse("(X?, Y?).self", ExprSyntax.parse)
    assertParse("([X], [Y]).self", ExprSyntax.parse)
    assertParse("([A : B], [C : D]).self", ExprSyntax.parse)

    assertParse("(G<A>.B, G<C>.D).self", ExprSyntax.parse)
    assertParse("(A?.B, C?.D).self", ExprSyntax.parse)
    assertParse("([A].B, [C].D).self", ExprSyntax.parse)
    assertParse("([A : B].C, [D : E].F).self", ExprSyntax.parse)

    assertParse("(X.Type, Y.Type).self", ExprSyntax.parse)
    assertParse("(X.Protocol, Y.Protocol).self", ExprSyntax.parse)

    assertParse("(P & Q, P & Q).self", ExprSyntax.parse)

    assertParse(
      """
      (
        (G<X>.Y) -> (P) & X?.Y, (X.Y, [X : Y?].Type), [(G<X>).Y], [A.B.C].D
      ).self
      """,
      ExprSyntax.parse
    )
  }

  "testFunctionTypeExpr" ignore AstFixture("") { cpg =>
    assertParse("X -> Y", ExprSyntax.parse)
    assertParse("(X) -> Y", ExprSyntax.parse)
    assertParse("(X) -> Y -> Z", ExprSyntax.parse)
    assertParse("P & Q -> X", ExprSyntax.parse)
    assertParse("A & B -> C & D -> X", ExprSyntax.parse)
    assertParse("(X -> Y).self", ExprSyntax.parse)
    assertParse("(A & B -> C & D).self", ExprSyntax.parse)

    assertParse("((X) -> Y).self", ExprSyntax.parse)
    assertParse("(((X)) -> (Y)).self", ExprSyntax.parse)

    assertParse("((A.B) -> C.D).self", ExprSyntax.parse)
    assertParse("(((A).B) -> (C).D).self", ExprSyntax.parse)

    assertParse("((G<X>) -> G<Y>).self", ExprSyntax.parse)
    assertParse("((X?) -> Y?).self", ExprSyntax.parse)
    assertParse("(([X]) -> [Y]).self", ExprSyntax.parse)
    assertParse("(([A : B]) -> [C : D]).self", ExprSyntax.parse)

    assertParse("((Gen<Foo>.Bar) -> Gen<Foo>.Bar).self", ExprSyntax.parse)
    assertParse("((Foo?.Bar) -> Foo?.Bar).self", ExprSyntax.parse)
    assertParse("(([Foo].Element) -> [Foo].Element).self", ExprSyntax.parse)
    assertParse("(([Int : Foo].Element) -> [Int : Foo].Element).self", ExprSyntax.parse)

    assertParse("((X.Type) -> Y.Type).self", ExprSyntax.parse)
    assertParse("((X.Protocol) -> Y.Protocol).self", ExprSyntax.parse)

    assertParse("(() -> X & Y).self", ExprSyntax.parse)
    assertParse("((A & B) -> C & D).self", ExprSyntax.parse)
    assertParse("((A & B) -> (C & D) -> E & Any).self", ExprSyntax.parse)

    assertParse(
      """
      (
        ((P) & X?.Y, G<X>.Y, (X, [A : B?].Type)) -> ([(X).Y]) -> [X].Y
      ).self
      """,
      ExprSyntax.parse
    )
  }

  "testTypeExpr27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func complexSequence() {
        // (assign_expr
        //   (discard_assignment_expr)
        //   (try_expr
        //     (type_expr typerepr='P1 & P2 throws -> P3 & P1')))
        _ = try P1 & P2 throws -> P3 & P1
      }
      """
    )
  }

  "testTypeExpr28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func takesVoid(f: 1️⃣Void 2️⃣-> ()) {}
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '(' to start function type", fixIts: ["insert '('"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected ')' in function type", fixIts: ["insert ')'"]),
      ],
      fixedSource: """
        func takesVoid(f: (Void) -> ()) {}
        """
    )
  }

  "testTypeExpr29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func takesOneArg<T>(_: T.Type) {}
      func takesTwoArgs<T>(_: T.Type, _: Int) {}
      """
    )
  }

  "testTypeExpr30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testMissingSelf" ignore AstFixture("") { cpg =>
        // None of these were not caught in Swift 3.
        // See test/Compatibility/type_expr.swift.
        takesOneArg(Int)
        takesOneArg(Swift.Int)
        takesTwoArgs(Int, 0)
        takesTwoArgs(Swift.Int, 0)
        Swift.Int
        _ = Swift.Int
      }
      """
    )
  }

}
