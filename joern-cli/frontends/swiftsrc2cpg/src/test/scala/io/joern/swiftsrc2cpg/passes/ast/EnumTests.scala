// This test file has been translated from swift/test/Parse/enum.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EnumTests extends AbstractPassTest {

  "EnumTests" should {

    "testEnum3" ignore AstFixture("enum Empty {}") { cpg => ??? }

    "testEnum4" ignore AstFixture("""
        |enum Boolish {
        |  case falsy
        |  case truthy
        |  init() { self = .falsy }
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum6" ignore AstFixture("""
        |enum Optionable<T> {
        |  case Nought
        |  case Mere(T)
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum8" ignore AstFixture("enum Color { case Red, Green, Grayscale(Int), Blue }") { cpg => ??? }

    "testEnum12" ignore AstFixture("""
        |struct SomeStruct {
        |  case StructCase
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum13" ignore AstFixture("""
        |class SomeClass {
        |  case ClassCase
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum14" ignore AstFixture("""
        |enum EnumWithExtension1 {
        |  case A1
        |}
        |extension EnumWithExtension1 {
        |  case A2
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum16" ignore AstFixture("""
        |enum EnumCaseAttributes {
        |  @xyz case EmptyAttributes
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum18" ignore AstFixture("""
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
        |""".stripMargin) { cpg => ??? }

    "testEnum19" ignore AstFixture("""
        |enum ImproperlyHasIVars {
        |  case Flopsy
        |  case Mopsy
        |  var ivar : Int
        |}""".stripMargin) { cpg => ??? }

    "testEnum21d" ignore AstFixture("""
        |enum Recovery4 {
        |  case `Self` Self
        |}""".stripMargin) { cpg => ??? }

    "testEnum22" ignore AstFixture("enum RawTypeEmpty : Int {}") { cpg => ??? }

    "testEnum23" ignore AstFixture("""
        |enum Raw : Int {
        |  case Ankeny, Burnside
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum24" ignore AstFixture("""
        |enum MultiRawType : Int64, Int32 {
        |  case Couch, Davis
        |}""".stripMargin) { cpg => ??? }

    "testEnum26" ignore AstFixture("""
        |enum ExpressibleByRawTypeNotLiteral : Array<Int> {
        |  case Ladd, Elliott, Sixteenth, Harrison
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum30" ignore AstFixture("""
        |enum RawTypeWithIntValues : Int {
        |  case Glisan = 17, Hoyt = 219, Irving, Johnson = 97209
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum31" ignore AstFixture("""
        |enum RawTypeWithNegativeValues : Int {
        |  case Glisan = -17, Hoyt = -219, Irving, Johnson = -97209
        |  case AutoIncAcrossZero = -1, Zero, One
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum37" ignore AstFixture("""
        |enum RawTypeWithStringValues : String {
        |  case Primrose // okay
        |  case Quimby = "Lucky Lab"
        |  case Raleigh // okay
        |  case Savier = "McMenamin's", Thurman = "Kenny and Zuke's"
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum38" ignore AstFixture("""
        |enum RawValuesWithoutRawType {
        |  case Upshur = 22
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum41" ignore AstFixture("""
        |enum RawTypeWithRepeatValues3 : Double {
        |  // 2^63-1
        |  case Vaughn = 9223372036854775807
        |  case Wilson = 9223372036854775807.0
        |}""".stripMargin) { cpg => ??? }

    "testEnum42" ignore AstFixture("""
        |enum RawTypeWithRepeatValues4 : Double {
        |  // 2^64-1
        |  case Vaughn = 18446744073709551615
        |  case Wilson = 18446744073709551615.0
        |}""".stripMargin) { cpg => ??? }

    "testEnum54" ignore AstFixture("""
        |enum NonliteralRawValue : Int {
        |  case Yeon = 100 + 20 + 3
        |}""".stripMargin) { cpg => ??? }

    "testEnum55" ignore AstFixture("""
        |enum RawTypeWithPayload : Int {
        |  case Powell(Int)
        |  case Terwilliger(Int) = 17
        |}""".stripMargin) { cpg => ??? }

    "testEnum72" ignore AstFixture("""
        |public protocol RawValueB {
        |  var rawValue: Double { get }
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum73" ignore AstFixture("""
        |enum RawValueBTest: Double, RawValueB {
        |  case A, B
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum74" ignore AstFixture("""
       |enum foo : String {
       |  case bar = nil
       |}""".stripMargin) { cpg => ??? }

    "testEnum77" ignore AstFixture("""
        |enum EnumWithStaticMember {
        |  static let staticVar = EmptyStruct()
        |  func foo() {
        |    let _ = staticVar
        |  }
        |}
        |""".stripMargin) { cpg => ??? }

    "testEnum81b" ignore AstFixture("""
        |switch self {
        |  case A(_): break
        |}""".stripMargin) { cpg => ??? }

    "testEnum82" ignore AstFixture("enum `switch` {}") { cpg => ??? }

    "testEnum83" ignore AstFixture("""
        |enum SE0155 {
        |  case emptyArgs()
        |}""".stripMargin) { cpg => ??? }

  }

}
