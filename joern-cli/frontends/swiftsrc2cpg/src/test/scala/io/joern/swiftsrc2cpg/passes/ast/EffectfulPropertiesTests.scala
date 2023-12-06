// This test file has been translated from swift/test/Parse/effectful_properties.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EffectfulPropertiesTests extends AbstractPassTest {

  "EffectfulPropertiesTests" should {

    "testEffectfulProperties1" ignore AstFixture("""
        |struct MyProps {
        |  var prop1 : Int {
        |    get async { }
        |  }
        |  var prop2 : Int {
        |    get throws { }
        |  }
        |  var prop3 : Int {
        |    get async throws { }
        |  }
        |  var prop1mut : Int {
        |    mutating get async { }
        |  }
        |  var prop2mut : Int {
        |    mutating get throws { }
        |  }
        |  var prop3mut : Int {
        |    mutating get async throws { }
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testEffectfulProperties2" ignore AstFixture("""
        |struct X1 {
        |  subscript(_ i : Int) -> Int {
        |    get async {}
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testEffectfulProperties3" ignore AstFixture("""
        |class X2 {
        |  subscript(_ i : Int) -> Int {
        |    get throws {}
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testEffectfulProperties4" ignore AstFixture("""
        |struct X3 {
        |  subscript(_ i : Int) -> Int {
        |    get async throws {}
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testEffectfulProperties5" ignore AstFixture("""
        |struct BadSubscript1 {
        |  subscript(_ i : Int) -> Int {
        |    get async throws {}
        |    set {}
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testEffectfulProperties6" ignore AstFixture("""
        |struct BadSubscript2 {
        |  subscript(_ i : Int) -> Int {
        |    get throws {}
        |    set throws {}
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testEffectfulProperties7" ignore AstFixture("""
        |struct S {
        |  var prop2 : Int {
        |    mutating get async throws { 0 }
        |    nonmutating set {}
        |  }
        |}
        |""".stripMargin) { cpg => ??? }

    "testEffectfulProperties8" ignore AstFixture("""
        |var prop3 : Bool {
        |  _read { yield prop3 }
        |  get throws { false }
        |  get async { true }
        |  get {}
        |}
        |""".stripMargin) { cpg => ??? }

    "testEffectfulProperties9" ignore AstFixture("""
        |enum E {
        |  private(set) var prop4 : Double {
        |    set {}
        |    get async throws { 1.1 }
        |    _modify { yield &prop4 }
        |  }
        |}
        |""".stripMargin) { cpg => ??? }

    "testEffectfulProperties10" ignore AstFixture("""
        |protocol P {
        |  associatedtype T
        |  var prop1 : T { get async throws }
        |  var prop2 : T { get async throws set }
        |  var prop3 : T { get throws set }
        |  var prop4 : T { get async }
        |  var prop5 : T { mutating get async throws }
        |  var prop6 : T { mutating get throws }
        |  var prop7 : T { mutating get async nonmutating set }
        |}
        |""".stripMargin) { cpg => ??? }

    "testEffectfulProperties14" ignore AstFixture("""
        |var bad3 : Int {
        |  _read async { yield 0 }
        |  set(theValue) async { }
        |}
        |""".stripMargin) { cpg => ??? }

  }

}
