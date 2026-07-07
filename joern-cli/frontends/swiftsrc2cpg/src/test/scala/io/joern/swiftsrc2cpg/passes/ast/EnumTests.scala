// This test file has been translated from swift/test/Parse/enum.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class EnumTests extends SwiftSrc2CpgSuite {

  "EnumTests" should {

    "testEnum3" in {
      val cpg            = code("enum Empty {}")
      val List(enumDecl) = cpg.typeDecl.nameExact("Empty").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Empty"
      enumDecl.member.l shouldBe empty
      enumDecl.inheritsFromTypeFullName.l shouldBe empty
      val List(initMethod) = enumDecl.method.isConstructor.l
      initMethod.fullName shouldBe "Test0.swift:<global>.Empty.init:()->Test0.swift:<global>.Empty"
    }

    "testEnum4" in {
      val cpg = code("""
        |enum Boolish {
        |  case falsy
        |  case truthy
        |  init() { self = .falsy }
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("Boolish").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Boolish"
      enumDecl.member.name.l shouldBe List("falsy", "truthy")
      val List(initMethod) = enumDecl.method.isConstructor.l
      initMethod.fullName shouldBe "Test0.swift:<global>.Boolish.init:()->Test0.swift:<global>.Boolish"
      initMethod.block.astChildren.isCall.code.l should contain("self = .falsy")
    }

    "testEnum6" in {
      val cpg = code("""
        |enum Optionable<T> {
        |  case Nought
        |  case Mere(T)
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("Optionable").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Optionable"
      enumDecl.member.name.l shouldBe List("Nought", "Mere")
    }

    "testEnum8" in {
      val cpg            = code("enum Color { case Red, Green, Grayscale(Int), Blue }")
      val List(enumDecl) = cpg.typeDecl.nameExact("Color").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Color"
      enumDecl.member.name.l shouldBe List("Red", "Green", "Grayscale", "Blue")
    }

    "testEnum12" in {
      val cpg = code("""
        |struct SomeStruct {
        |  case StructCase
        |}
        |""".stripMargin)
      // Even though `case` inside a struct is invalid Swift, the parser recovers and the case becomes a member.
      val List(structDecl) = cpg.typeDecl.nameExact("SomeStruct").l
      structDecl.fullName shouldBe "Test0.swift:<global>.SomeStruct"
      structDecl.member.name.l shouldBe List("StructCase")
    }

    "testEnum13" in {
      val cpg = code("""
        |class SomeClass {
        |  case ClassCase
        |}
        |""".stripMargin)
      val List(classDecl) = cpg.typeDecl.nameExact("SomeClass").l
      classDecl.fullName shouldBe "Test0.swift:<global>.SomeClass"
      classDecl.member.name.l shouldBe List("ClassCase")
    }

    "testEnum14" in {
      val cpg = code("""
        |enum EnumWithExtension1 {
        |  case A1
        |}
        |extension EnumWithExtension1 {
        |  case A2
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("EnumWithExtension1").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.EnumWithExtension1"
      // Only the original enum body case lands as a member; the extension's case is ignored at member-level.
      enumDecl.member.name.l shouldBe List("A1")
    }

    "testEnum16" in {
      val cpg = code("""
        |enum EnumCaseAttributes {
        |  @xyz case EmptyAttributes
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("EnumCaseAttributes").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.EnumCaseAttributes"
      enumDecl.member.name.l shouldBe List("EmptyAttributes")
    }

    "testEnum18" in {
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
      val List(enumDecl) = cpg.typeDecl.nameExact("HasMethodsPropertiesAndCtors").l
      enumDecl.member.name.l shouldBe List("TweedleDee", "TweedleDum", "property")
      enumDecl.method.name.l shouldBe List("init", "method", "staticMethod", "subscript", "property")
      enumDecl.method.nameExact("subscript").fullName.head shouldBe
        "Test0.swift:<global>.HasMethodsPropertiesAndCtors.subscript:(x:Swift.Int)->Swift.Int"
      enumDecl.method.nameExact("property").fullName.head shouldBe
        "Test0.swift:<global>.HasMethodsPropertiesAndCtors.property:Swift.Int"
    }

    "testEnum19" in {
      val cpg = code("""
        |enum ImproperlyHasIVars {
        |  case Flopsy
        |  case Mopsy
        |  var ivar : Int
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("ImproperlyHasIVars").l
      enumDecl.member.name.l shouldBe List("Flopsy", "Mopsy", "ivar")
      val List(ivarMember) = enumDecl.member.nameExact("ivar").l
      ivarMember.typeFullName shouldBe "Swift.Int"
    }

    "testEnum21d" in {
      val cpg = code("""
        |enum Recovery4 {
        |  case `Self` Self
        |}""".stripMargin)
      // Recovery: malformed enum should still produce the enum's TypeDecl with at least the `Self` case member.
      val List(enumDecl) = cpg.typeDecl.nameExact("Recovery4").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Recovery4"
      enumDecl.member.name.l should contain("`Self`")
    }

    "testEnum22" in {
      val cpg            = code("enum RawTypeEmpty : Int {}")
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeEmpty").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.RawTypeEmpty"
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Int")
      enumDecl.member.l shouldBe empty
    }

    "testEnum23" in {
      val cpg = code("""
        |enum Raw : Int {
        |  case Ankeny, Burnside
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("Raw").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.Raw"
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Int")
      enumDecl.member.name.l shouldBe List("Ankeny", "Burnside")
    }

    "testEnum24" in {
      val cpg = code("""
        |enum MultiRawType : Int64, Int32 {
        |  case Couch, Davis
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("MultiRawType").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.MultiRawType"
      enumDecl.inheritsFromTypeFullName.l should contain allOf ("Int64", "Int32")
      enumDecl.member.name.l shouldBe List("Couch", "Davis")
    }

    "testEnum26" in {
      val cpg = code("""
        |enum ExpressibleByRawTypeNotLiteral : Array<Int> {
        |  case Ladd, Elliott, Sixteenth, Harrison
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("ExpressibleByRawTypeNotLiteral").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.ExpressibleByRawTypeNotLiteral"
      enumDecl.inheritsFromTypeFullName.l should contain("Swift.Array")
      enumDecl.member.name.l shouldBe List("Ladd", "Elliott", "Sixteenth", "Harrison")
    }

    "testEnum30" in {
      val cpg = code("""
        |enum RawTypeWithIntValues : Int {
        |  case Glisan = 17, Hoyt = 219, Irving, Johnson = 97209
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeWithIntValues").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Int")
      enumDecl.member.name.l shouldBe List("Glisan", "Hoyt", "Irving", "Johnson")
    }

    "testEnum31" in {
      val cpg = code("""
        |enum RawTypeWithNegativeValues : Int {
        |  case Glisan = -17, Hoyt = -219, Irving, Johnson = -97209
        |  case AutoIncAcrossZero = -1, Zero, One
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeWithNegativeValues").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Int")
      enumDecl.member.name.l shouldBe List("Glisan", "Hoyt", "Irving", "Johnson", "AutoIncAcrossZero", "Zero", "One")
    }

    "testEnum37" in {
      val cpg = code("""
        |enum RawTypeWithStringValues : String {
        |  case Primrose // okay
        |  case Quimby = "Lucky Lab"
        |  case Raleigh // okay
        |  case Savier = "McMenamin's", Thurman = "Kenny and Zuke's"
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeWithStringValues").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.String")
      enumDecl.member.name.l.sorted shouldBe List("Primrose", "Quimby", "Raleigh", "Savier", "Thurman")
    }

    "testEnum38" in {
      val cpg = code("""
        |enum RawValuesWithoutRawType {
        |  case Upshur = 22
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawValuesWithoutRawType").l
      enumDecl.inheritsFromTypeFullName.l shouldBe empty
      enumDecl.member.name.l shouldBe List("Upshur")
    }

    "testEnum41" in {
      val cpg = code("""
        |enum RawTypeWithRepeatValues3 : Double {
        |  // 2^63-1
        |  case Vaughn = 9223372036854775807
        |  case Wilson = 9223372036854775807.0
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeWithRepeatValues3").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Double")
      enumDecl.member.name.l shouldBe List("Vaughn", "Wilson")
    }

    "testEnum42" in {
      val cpg = code("""
        |enum RawTypeWithRepeatValues4 : Double {
        |  // 2^64-1
        |  case Vaughn = 18446744073709551615
        |  case Wilson = 18446744073709551615.0
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeWithRepeatValues4").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Double")
      enumDecl.member.name.l shouldBe List("Vaughn", "Wilson")
    }

    "testEnum54" in {
      val cpg = code("""
        |enum NonliteralRawValue : Int {
        |  case Yeon = 100 + 20 + 3
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("NonliteralRawValue").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Int")
      enumDecl.member.name.l shouldBe List("Yeon")
    }

    "testEnum55" in {
      val cpg = code("""
        |enum RawTypeWithPayload : Int {
        |  case Powell(Int)
        |  case Terwilliger(Int) = 17
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawTypeWithPayload").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.Int")
      enumDecl.member.name.l.sorted shouldBe List("Powell", "Terwilliger")
    }

    "testEnum72" in {
      val cpg = code("""
        |public protocol RawValueB {
        |  var rawValue: Double { get }
        |}
        |""".stripMargin)
      val List(protoDecl) = cpg.typeDecl.nameExact("RawValueB").l
      protoDecl.fullName shouldBe "Test0.swift:<global>.RawValueB"
      protoDecl.modifier.modifierType.l should contain(ModifierTypes.PUBLIC)
      protoDecl.member.name.l shouldBe List("rawValue")
    }

    "testEnum73" in {
      val cpg = code("""
        |enum RawValueBTest: Double, RawValueB {
        |  case A, B
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("RawValueBTest").l
      enumDecl.inheritsFromTypeFullName.l should contain allOf ("Swift.Double", "RawValueB")
      enumDecl.member.name.l shouldBe List("A", "B")
    }

    "testEnum74" in {
      val cpg = code("""
       |enum foo : String {
       |  case bar = nil
       |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("foo").l
      enumDecl.inheritsFromTypeFullName.l shouldBe List("Swift.String")
      enumDecl.member.name.l shouldBe List("bar")
    }

    "testEnum77" in {
      val cpg = code("""
        |enum EnumWithStaticMember {
        |  static let staticVar = EmptyStruct()
        |  func foo() {
        |    let _ = staticVar
        |  }
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("EnumWithStaticMember").l
      enumDecl.member.name.l should contain("staticVar")
      enumDecl.method.name.l should contain("foo")
    }

    "testEnum81b" in {
      val cpg = code("""
        |switch self {
        |  case A(_): break
        |}""".stripMargin)
      val List(switchStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchStructure.code should startWith("switch self")
    }

    "testEnum82" in {
      val cpg            = code("enum `switch` {}")
      val List(enumDecl) = cpg.typeDecl.filename("Test0.swift").nameExact("`switch`").l
      enumDecl.fullName shouldBe "Test0.swift:<global>.`switch`"
    }

    "testEnum83" in {
      val cpg = code("""
        |enum SE0155 {
        |  case emptyArgs()
        |}""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("SE0155").l
      enumDecl.member.name.l shouldBe List("emptyArgs")
    }

    "nested enum should not produce dangling TypeRefs" in {
      val cpg = code("""
          |class Foo: FooParent {
          |  enum SE0155: SParent {
          |    case emptyArgs()
          |  }
          |}""".stripMargin)
      cpg.typeRef.filter(_.astIn.isEmpty) shouldBe empty
    }

  }

}
