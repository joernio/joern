

// This test file has been translated from swift/test/Parse/enum.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EnumTests extends AbstractPassTest {
  "testEnum1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // FIXME: this test only passes on platforms which have Float80.
      // <rdar://problem/19508460> Floating point enum raw values are not portable
      """
    )
  }

  "testEnum2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Windows does not support FP80
      // XFAIL: OS=windows-msvc
      """
    )
  }

  "testEnum3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Empty {}
      """
    )
  }

  "testEnum4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Boolish {
        case falsy
        case truthy
        init() { self = .falsy }
      }
      """
    )
  }

  "testEnum5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var b = Boolish.falsy
      b = .truthy
      """
    )
  }

  "testEnum6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Optionable<T> {
        case Nought
        case Mere(T)
      }
      """
    )
  }

  "testEnum7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var o = Optionable<Int>.Nought
      o = .Mere(0)
      """
    )
  }

  "testEnum8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Color { case Red, Green, Grayscale(Int), Blue }
      """
    )
  }

  "testEnum9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var c = Color.Red
      c = .Green
      c = .Grayscale(255)
      c = .Blue
      """
    )
  }

  "testEnum10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let partialApplication = Color.Grayscale
      """
    )
  }

  "testEnum11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Cases are excluded from non-enums.
      1️⃣case FloatingCase
      """,
      diagnostics: [
        DiagnosticSpec(message: "'case' can only appear inside a 'switch' statement or 'enum' declaration")
      ]
    )
  }

  "testEnum12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct SomeStruct {
        case StructCase
      }
      """
    )
  }

  "testEnum13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class SomeClass {
        case ClassCase
      }
      """
    )
  }

  "testEnum14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum EnumWithExtension1 {
        case A1
      }
      extension EnumWithExtension1 {
        case A2
      }
      """
    )
  }

  "testEnum15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Attributes for enum cases.
      """
    )
  }

  "testEnum16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum EnumCaseAttributes {
        @xyz case EmptyAttributes
      }
      """
    )
  }

  // Recover when a switch 'case' label is spelled inside an enum (or outside).
  "testEnum17a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case X1️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ':' in enum")
      ]
    )
  }

  "testEnum17b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case X(Y)1️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ':' in enum")
      ]
    )
  }

  "testEnum17c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case X, Y1️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ':' in enum")
      ]
    )
  }

  "testEnum17d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case X 1️⃣where true:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'where true:' in enum")
      ]
    )
  }

  "testEnum17e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case X(Y), Z(W)1️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ':' in enum")
      ]
    )
  }

  "testEnum17f" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case X(Y) 1️⃣where true:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'where true:' in enum")
      ]
    )
  }

  "testEnum17g" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case 1️⃣02️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "identifier can only start with a letter or underscore, not a number"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code ':' in enum"),
      ]
    )
  }

  "testEnum17h" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case 1️⃣_2️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'_' cannot be used as an identifier here"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code ':' in enum"),
      ]
    )
  }

  "testEnum17i" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SwitchEnvy {
        case 1️⃣(_, var x2️⃣, 3️⃣0)4️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected identifier in enum case", fixIts: ["insert identifier"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected ':' and type in parameter", fixIts: ["insert ':' and type"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "unexpected code '0' in parameter clause"),
        DiagnosticSpec(locationMarker: "4️⃣", message: "unexpected code ':' in enum"),
      ],
      fixedSource: """
        enum SwitchEnvy {
          case <#identifier#>(_, var x: <#type#>, 0):
        }
        """
    )
  }

  "testEnum18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum HasMethodsPropertiesAndCtors {
        case TweedleDee
        case TweedleDum
        func method() {}
        func staticMethod() {}
        init() {}
        subscript(x:Int) -> Int {
          return 0
        }
        var property : Int {
          return 0
        }
      }
      """
    )
  }

  "testEnum19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum ImproperlyHasIVars {
        case Flopsy
        case Mopsy
        var ivar : Int
      }
      """
    )
  }

  "testEnum20" ignore AstFixture("") { cpg =>
    // We used to crash on this.  rdar://14678675
    assertParse(
      """
      enum rdar14678675 {
        case U1, 1️⃣
        case U2
        case U3
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in enum case", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        enum rdar14678675 {
          case U1, <#identifier#>
          case U2
          case U3
        }
        """
    )
  }

  "testEnum21a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Recovery1 {
        case1️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in enum case", fixIts: ["insert identifier"]),
        DiagnosticSpec(message: "unexpected code ':' in enum"),
      ],
      fixedSource: """
        enum Recovery1 {
          case <#identifier#>:
        }
        """
    )
  }

  "testEnum21b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Recovery2 {
        case UE11️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ':' in enum")
      ]
    )
  }

  "testEnum21c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Recovery3 {
        case UE2(Void)1️⃣:
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ':' in enum")
      ]
    )
  }

  "testEnum21d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Recovery4 {
        case 1️⃣Self 2️⃣Self
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "keyword 'Self' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected 'Self' keyword in enum"),
      ],
      fixedSource: """
        enum Recovery4 {
          case `Self` Self
        }
        """
    )
  }

  "testEnum21e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Recovery5 {
        case 1️⃣.UE3
        case 2️⃣.UE4, 3️⃣.UE5
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code '.' in enum case"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '.' in enum case"),
        DiagnosticSpec(locationMarker: "3️⃣", message: "unexpected code '.' in enum case"),
      ]
    )
  }

  "testEnum21f" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Recovery6 {
        case Snout, 1️⃣_;
        case 2️⃣_;
        case Tusk, 3️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'_' cannot be used as an identifier here"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "'_' cannot be used as an identifier here"),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected identifier in enum case", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        enum Recovery6 {
          case Snout, _;
          case _;
          case Tusk, <#identifier#>
        }
        """
    )
  }

  "testEnum22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeEmpty : Int {}
      """
    )
  }

  "testEnum23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Raw : Int {
        case Ankeny, Burnside
      }
      """
    )
  }

  "testEnum24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum MultiRawType : Int64, Int32 {
        case Couch, Davis
      }
      """
    )
  }

  "testEnum25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol RawTypeNotFirstProtocol {}
      enum RawTypeNotFirst : RawTypeNotFirstProtocol, Int {
        case E
      }
      """
    )
  }

  "testEnum26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum ExpressibleByRawTypeNotLiteral : Array<Int> {
        case Ladd, Elliott, Sixteenth, Harrison
      }
      """
    )
  }

  "testEnum27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeCircularityA : RawTypeCircularityB, ExpressibleByIntegerLiteral {
        case Morrison, Belmont, Madison, Hawthorne
        init(integerLiteral value: Int) {
          self = .Morrison
        }
      }
      """
    )
  }

  "testEnum28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeCircularityB : RawTypeCircularityA, ExpressibleByIntegerLiteral {
        case Willamette, Columbia, Sandy, Multnomah
        init(integerLiteral value: Int) {
          self = .Willamette
        }
      }
      """
    )
  }

  "testEnum29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct ExpressibleByFloatLiteralOnly : ExpressibleByFloatLiteral {
          init(floatLiteral: Double) {}
      }
      enum ExpressibleByRawTypeNotIntegerLiteral : ExpressibleByFloatLiteralOnly {
        case Everett
        case Flanders
      }
      """
    )
  }

  "testEnum30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithIntValues : Int {
        case Glisan = 17, Hoyt = 219, Irving, Johnson = 97209
      }
      """
    )
  }

  "testEnum31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithNegativeValues : Int {
        case Glisan = -17, Hoyt = -219, Irving, Johnson = -97209
        case AutoIncAcrossZero = -1, Zero, One
      }
      """
    )
  }

  "testEnum32" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithUnicodeScalarValues : UnicodeScalar {
        case Kearney = "K"
        case Lovejoy
        case Marshall = "M"
      }
      """#
    )
  }

  "testEnum33" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithCharacterValues : Character {
        case First = "い"
        case Second
        case Third = "は"
      }
      """#
    )
  }

  "testEnum34" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithCharacterValues_Correct : Character {
        case First = "😅" // ok
        case Second = "👩‍👩‍👧‍👦" // ok
        case Third = "👋🏽" // ok
        case Fourth = "\u{1F3F4}\u{E0067}\u{E0062}\u{E0065}\u{E006E}\u{E0067}\u{E007F}" // ok
      }
      """#
    )
  }

  "testEnum35" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithCharacterValues_Error1 : Character {
        case First = "abc"
      }
      """#
    )
  }

  "testEnum36" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithFloatValues : Float {
        case Northrup = 1.5
        case Overton
        case Pettygrove = 2.25
      }
      """
    )
  }

  "testEnum37" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithStringValues : String {
        case Primrose // okay
        case Quimby = "Lucky Lab"
        case Raleigh // okay
        case Savier = "McMenamin's", Thurman = "Kenny and Zuke's"
      }
      """#
    )
  }

  "testEnum38" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawValuesWithoutRawType {
        case Upshur = 22
      }
      """
    )
  }

  "testEnum39" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues : Int {
        case Vaughn = 22
        case Wilson = 22
      }
      """
    )
  }

  "testEnum40" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues2 : Double {
        case Vaughn = 22
        case Wilson = 22.0
      }
      """
    )
  }

  "testEnum41" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues3 : Double {
        // 2^63-1
        case Vaughn = 9223372036854775807
        case Wilson = 9223372036854775807.0
      }
      """
    )
  }

  "testEnum42" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues4 : Double {
        // 2^64-1
        case Vaughn = 18446744073709551615
        case Wilson = 18446744073709551615.0
      }
      """
    )
  }

  "testEnum43" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues5 : Double {
        // FIXME: should reject.
        // 2^65-1
        case Vaughn = 36893488147419103231
        case Wilson = 36893488147419103231.0
      }
      """
    )
  }

  "testEnum44" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues6 : Double {
        // FIXME: should reject.
        // 2^127-1
        case Vaughn = 170141183460469231731687303715884105727
        case Wilson = 170141183460469231731687303715884105727.0
      }
      """
    )
  }

  "testEnum45" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValues7 : Double {
        // FIXME: should reject.
        // 2^128-1
        case Vaughn = 340282366920938463463374607431768211455
        case Wilson = 340282366920938463463374607431768211455.0
      }
      """
    )
  }

  "testEnum46" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithRepeatValues8 : String {
        case Vaughn = "XYZ"
        case Wilson = "XYZ"
      }
      """#
    )
  }

  "testEnum47" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithNonRepeatValues : Double {
        case SantaClara = 3.7
        case SanFernando = 7.4
        case SanAntonio = -3.7
        case SanCarlos = -7.4
      }
      """
    )
  }

  "testEnum48" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValuesAutoInc : Double {
        case Vaughn = 22
        case Wilson
        case Yeon = 23
      }
      """
    )
  }

  "testEnum49" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValuesAutoInc2 : Double {
        case Vaughn = 23
        case Wilson = 22
        case Yeon
      }
      """
    )
  }

  "testEnum50" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithRepeatValuesAutoInc3 : Double {
        case Vaughn
        case Wilson
        case Yeon = 1
      }
      """
    )
  }

  "testEnum51" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithRepeatValuesAutoInc4 : String {
        case A = "B"
        case B
      }
      """#
    )
  }

  "testEnum52" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithRepeatValuesAutoInc5 : String {
        case A
        case B = "A"
      }
      """#
    )
  }

  "testEnum53" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeWithRepeatValuesAutoInc6 : String {
        case A
        case B
        case C = "B"
      }
      """#
    )
  }

  "testEnum54" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum NonliteralRawValue : Int {
        case Yeon = 100 + 20 + 3
      }
      """
    )
  }

  "testEnum55" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawTypeWithPayload : Int {
        case Powell(Int)
        case Terwilliger(Int) = 17
      }
      """
    )
  }

  "testEnum56" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum RawTypeMismatch : Int {
        case Barbur = "foo"
      }
      """#
    )
  }

  "testEnum57" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum DuplicateMembers1 {
        case Foo
        case Foo
      }
      """
    )
  }

  "testEnum58" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum DuplicateMembers2 {
        case Foo, Bar
        case Foo
        case Bar
      }
      """
    )
  }

  "testEnum59" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum DuplicateMembers3 {
        case Foo
        case Foo(Int)
      }
      """
    )
  }

  "testEnum60" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum DuplicateMembers4 : Int {
        case Foo = 1
        case Foo = 2
      }
      """
    )
  }

  "testEnum61" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum DuplicateMembers5 : Int {
        case Foo = 1
        case Foo = 1 + 1
      }
      """
    )
  }

  "testEnum62" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum DuplicateMembers6 {
        case Foo // expected-note 2{{'Foo' previously declared here}}
        case Foo
        case Foo
      }
      """
    )
  }

  "testEnum63" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum DuplicateMembers7 : String {
        case Foo
        case Foo = "Bar"
      }
      """#
    )
  }

  "testEnum64" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // Refs to duplicated enum cases shouldn't crash the compiler.
      // rdar://problem/20922401
      func check20922401() -> String {
        let x: DuplicateMembers1 = .Foo
        switch x {
          case .Foo:
            return "Foo"
        }
      }
      """#
    )
  }

  "testEnum65" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum PlaygroundRepresentation : UInt8 {
        case Class = 1
        case Struct = 2
        case Tuple = 3
        case Enum = 4
        case Aggregate = 5
        case Container = 6
        case IDERepr = 7
        case Gap = 8
        case ScopeEntry = 9
        case ScopeExit = 10
        case Error = 11
        case IndexContainer = 12
        case KeyContainer = 13
        case MembershipContainer = 14
        case Unknown = 0xFF
        static func fromByte(byte : UInt8) -> PlaygroundRepresentation {
          let repr = PlaygroundRepresentation(rawValue: byte)
          if repr == .none { return .Unknown } else { return repr! }
        }
      }
      """
    )
  }

  "testEnum66" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct ManyLiteralable : ExpressibleByIntegerLiteral, ExpressibleByStringLiteral, Equatable {
        init(stringLiteral: String) {}
        init(integerLiteral: Int) {}
        init(unicodeScalarLiteral: String) {}
        init(extendedGraphemeClusterLiteral: String) {}
      }
      func ==(lhs: ManyLiteralable, rhs: ManyLiteralable) -> Bool { return true }
      """
    )
  }

  "testEnum67" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum ManyLiteralA : ManyLiteralable {
        case A
        case B = 0
      }
      """
    )
  }

  "testEnum68" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum ManyLiteralB : ManyLiteralable {
        case A = "abc"
        case B
      }
      """#
    )
  }

  "testEnum69" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum ManyLiteralC : ManyLiteralable {
        case A
        case B = "0"
      }
      """#
    )
  }

  "testEnum70" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // rdar://problem/22476643
      public protocol RawValueA: RawRepresentable
      {
        var rawValue: Double { get }
      }
      """
    )
  }

  "testEnum71" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawValueATest: Double, RawValueA {
        case A, B
      }
      """
    )
  }

  "testEnum72" ignore AstFixture("") { cpg =>
    assertParse(
      """
      public protocol RawValueB
      {
        var rawValue: Double { get }
      }
      """
    )
  }

  "testEnum73" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum RawValueBTest: Double, RawValueB {
        case A, B
      }
      """
    )
  }

  "testEnum74" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum foo : String {
        case bar = nil
      }
      """
    )
  }

  "testEnum75" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Static member lookup from instance methods
      """
    )
  }

  "testEnum76" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct EmptyStruct {}
      """
    )
  }

  "testEnum77" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum EnumWithStaticMember {
        static let staticVar = EmptyStruct()
        func foo() {
          let _ = staticVar
        }
      }
      """
    )
  }

  "testEnum78" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // SE-0036:
      """
    )
  }

  "testEnum79" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct SE0036_Auxiliary {}
      """
    )
  }

  "testEnum80" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SE0036 {
        case A
        case B(SE0036_Auxiliary)
        case C(SE0036_Auxiliary)
        static func staticReference() {
          _ = A
          _ = self.A
          _ = SE0036.A
        }
        func staticReferenceInInstanceMethod() {
          _ = A
          _ = self.A
          _ = SE0036.A
        }
        static func staticReferenceInSwitchInStaticMethod() {
          switch SE0036.A {
          case A: break
          case B(_): break
          case C(let x): _ = x; break
          }
        }
        func staticReferenceInSwitchInInstanceMethod() {
          switch self {
          case A: break
          case B(_): break
          case C(let x): _ = x; break
          }
        }
        func explicitReferenceInSwitch() {
          switch SE0036.A {
          case SE0036.A: break
          case SE0036.B(_): break
          case SE0036.C(let x): _ = x; break
          }
        }
        func dotReferenceInSwitchInInstanceMethod() {
          switch self {
          case .A: break
          case .B(_): break
          case .C(let x): _ = x; break
          }
        }
        static func dotReferenceInSwitchInStaticMethod() {
          switch SE0036.A {
          case .A: break
          case .B(_): break
          case .C(let x): _ = x; break
          }
        }
        init() {
          self = .A
          self = A
          self = SE0036.A
          self = .B(SE0036_Auxiliary())
          self = B(SE0036_Auxiliary())
          self = SE0036.B(SE0036_Auxiliary())
        }
      }
      """
    )
  }

  "testEnum81" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SE0036_Generic<T> {
        case A(x: T)
        func foo() {
          switch self {
          case A(_): break
          }
          switch self {
          case .A(let a): print(a)
          }
          switch self {
          case SE0036_Generic.A(let a): print(a)
          }
        }
      }
      """
    )
  }

  "testEnum81b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch self {
        case A(_): break
      }
      """
    )
  }

  "testEnum82" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum 1️⃣switch {}
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "keyword 'switch' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        )
      ],
      fixedSource: """
        enum `switch` {}
        """
    )
  }

  "testEnum83" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum SE0155 {
        case emptyArgs()
      }
      """
    )
  }

  "testEnum84" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/53662
      """
    )
  }

  "testEnum85" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662 {
        case identifier
        case 1️⃣operator
        case identifier2
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "keyword 'operator' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        )
      ],
      fixedSource: """
        enum E_53662 {
          case identifier
          case `operator`
          case identifier2
        }
        """
    )
  }

  "testEnum86" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662_var {
        case identifier
        case 1️⃣var
        case identifier2
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "keyword 'var' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        )
      ],
      fixedSource: """
        enum E_53662_var {
          case identifier
          case `var`
          case identifier2
        }
        """
    )
  }

  "testEnum87" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662_underscore {
        case identifier
        case 1️⃣_
        case identifier2
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'_' cannot be used as an identifier here")
      ]
    )
  }

  "testEnum88" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662_Comma {
        case a, b, c, 1️⃣func, d
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "keyword 'func' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        )
      ],
      fixedSource: """
        enum E_53662_Comma {
          case a, b, c, `func`, d
        }
        """
    )
  }

  "testEnum89" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662_Newline {
        case identifier1
        case identifier2
        case 1️⃣
        case identifier
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in enum case", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        enum E_53662_Newline {
          case identifier1
          case identifier2
          case <#identifier#>
          case identifier
        }
        """
    )
  }

  "testEnum90" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662_Newline2 {
        case 1️⃣
        func foo() {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in enum case", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        enum E_53662_Newline2 {
          case <#identifier#>
          func foo() {}
        }
        """
    )
  }

  "testEnum91" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E_53662_PatternMatching {
        case 1️⃣let 2️⃣.foo(x, y):
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "keyword 'let' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "unexpected code '.foo(x, y):' in enum"
        ),
      ],
      fixedSource: """
        enum E_53662_PatternMatching {
          case `let` .foo(x, y):
        }
        """
    )
  }

  "testEnum92" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum CasesWithMissingElement: Int {
        case a = "hello", 1️⃣
        case b = "hello", 2️⃣
      }
      """#,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected identifier in enum case", fixIts: ["insert identifier"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in enum case", fixIts: ["insert identifier"]),
      ],
      fixedSource: #"""
        enum CasesWithMissingElement: Int {
          case a = "hello", <#identifier#>
          case b = "hello", <#identifier#>
        }
        """#
    )
  }

  "testEnumCaseWithWildcardAsFirstName" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum Foo {
        case a(_ x: Int)
      }
      """#
    )
  }

  func parseEnumCaseElementParameterOnNewline() {
    assertParse(
      """
      enum E {
        case a
          (Int)
      }
      """
    )
  }
}
