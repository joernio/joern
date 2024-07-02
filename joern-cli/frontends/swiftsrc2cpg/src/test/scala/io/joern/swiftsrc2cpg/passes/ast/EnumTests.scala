// This test file has been translated from swift/test/Parse/enum.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class EnumTests extends AstSwiftSrc2CpgSuite {

  "EnumTests" should {

    "testEnum3" ignore {
      val cpg = code("enum Empty {}")
      ???
    }

    "testEnum4" ignore {
      val cpg = code("""
        |enum Boolish {
        |  case falsy
        |  case truthy
        |  init() { self = .falsy }
        |}
        |""".stripMargin)
      ???
    }

    "testEnum6" ignore {
      val cpg = code("""
        |enum Optionable<T> {
        |  case Nought
        |  case Mere(T)
        |}
        |""".stripMargin)
      ???
    }

    "testEnum8" ignore {
      val cpg = code("enum Color { case Red, Green, Grayscale(Int), Blue }")
      ???
    }

    "testEnum12" ignore {
      val cpg = code("""
        |struct SomeStruct {
        |  case StructCase
        |}
        |""".stripMargin)
      ???
    }

    "testEnum13" ignore {
      val cpg = code("""
        |class SomeClass {
        |  case ClassCase
        |}
        |""".stripMargin)
      ???
    }

    "testEnum14" ignore {
      val cpg = code("""
        |enum EnumWithExtension1 {
        |  case A1
        |}
        |extension EnumWithExtension1 {
        |  case A2
        |}
        |""".stripMargin)
      ???
    }

    "testEnum16" ignore {
      val cpg = code("""
        |enum EnumCaseAttributes {
        |  @xyz case EmptyAttributes
        |}
        |""".stripMargin)
      ???
    }

    "testEnum18" ignore {
      val cpg = code("""
        |enum HasMethodsPropertiesAndCtors {
        |  case TweedleDee
        |  case TweedleDum
        |  func method() {}
        |  func staticMethod() {}
        |  init() {}
        |  subscript(x:Int) -> Int {
        |    return 0
        |  }
        |  var property : Int {
        |    return 0
        |  }
        |}
        |""".stripMargin)
      ???
    }

    "testEnum19" ignore {
      val cpg = code("""
        |enum ImproperlyHasIVars {
        |  case Flopsy
        |  case Mopsy
        |  var ivar : Int
        |}""".stripMargin)
      ???
    }

    "testEnum21d" ignore {
      val cpg = code("""
        |enum Recovery4 {
        |  case `Self` Self
        |}""".stripMargin)
      ???
    }

    "testEnum22" ignore {
      val cpg = code("enum RawTypeEmpty : Int {}")
      ???
    }

    "testEnum23" ignore {
      val cpg = code("""
        |enum Raw : Int {
        |  case Ankeny, Burnside
        |}
        |""".stripMargin)
      ???
    }

    "testEnum24" ignore {
      val cpg = code("""
        |enum MultiRawType : Int64, Int32 {
        |  case Couch, Davis
        |}""".stripMargin)
      ???
    }

    "testEnum26" ignore {
      val cpg = code("""
        |enum ExpressibleByRawTypeNotLiteral : Array<Int> {
        |  case Ladd, Elliott, Sixteenth, Harrison
        |}
        |""".stripMargin)
      ???
    }

    "testEnum30" ignore {
      val cpg = code("""
        |enum RawTypeWithIntValues : Int {
        |  case Glisan = 17, Hoyt = 219, Irving, Johnson = 97209
        |}
        |""".stripMargin)
      ???
    }

    "testEnum31" ignore {
      val cpg = code("""
        |enum RawTypeWithNegativeValues : Int {
        |  case Glisan = -17, Hoyt = -219, Irving, Johnson = -97209
        |  case AutoIncAcrossZero = -1, Zero, One
        |}
        |""".stripMargin)
      ???
    }

    "testEnum37" ignore {
      val cpg = code("""
        |enum RawTypeWithStringValues : String {
        |  case Primrose // okay
        |  case Quimby = "Lucky Lab"
        |  case Raleigh // okay
        |  case Savier = "McMenamin's", Thurman = "Kenny and Zuke's"
        |}
        |""".stripMargin)
      ???
    }

    "testEnum38" ignore {
      val cpg = code("""
        |enum RawValuesWithoutRawType {
        |  case Upshur = 22
        |}
        |""".stripMargin)
      ???
    }

    "testEnum41" ignore {
      val cpg = code("""
        |enum RawTypeWithRepeatValues3 : Double {
        |  // 2^63-1
        |  case Vaughn = 9223372036854775807
        |  case Wilson = 9223372036854775807.0
        |}""".stripMargin)
      ???
    }

    "testEnum42" ignore {
      val cpg = code("""
        |enum RawTypeWithRepeatValues4 : Double {
        |  // 2^64-1
        |  case Vaughn = 18446744073709551615
        |  case Wilson = 18446744073709551615.0
        |}""".stripMargin)
      ???
    }

    "testEnum54" ignore {
      val cpg = code("""
        |enum NonliteralRawValue : Int {
        |  case Yeon = 100 + 20 + 3
        |}""".stripMargin)
      ???
    }

    "testEnum55" ignore {
      val cpg = code("""
        |enum RawTypeWithPayload : Int {
        |  case Powell(Int)
        |  case Terwilliger(Int) = 17
        |}""".stripMargin)
      ???
    }

    "testEnum72" ignore {
      val cpg = code("""
        |public protocol RawValueB {
        |  var rawValue: Double { get }
        |}
        |""".stripMargin)
      ???
    }

    "testEnum73" ignore {
      val cpg = code("""
        |enum RawValueBTest: Double, RawValueB {
        |  case A, B
        |}
        |""".stripMargin)
      ???
    }

    "testEnum74" ignore {
      val cpg = code("""
       |enum foo : String {
       |  case bar = nil
       |}""".stripMargin)
      ???
    }

    "testEnum77" ignore {
      val cpg = code("""
        |enum EnumWithStaticMember {
        |  static let staticVar = EmptyStruct()
        |  func foo() {
        |    let _ = staticVar
        |  }
        |}
        |""".stripMargin)
      ???
    }

    "testEnum81b" ignore {
      val cpg = code("""
        |switch self {
        |  case A(_): break
        |}""".stripMargin)
      ???
    }

    "testEnum82" ignore {
      val cpg = code("enum `switch` {}")
      ???
    }

    "testEnum83" ignore {
      val cpg = code("""
        |enum SE0155 {
        |  case emptyArgs()
        |}""".stripMargin)
      ???
    }

  }

}
