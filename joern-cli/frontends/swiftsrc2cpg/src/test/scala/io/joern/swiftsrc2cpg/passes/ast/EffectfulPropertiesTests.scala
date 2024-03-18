// This test file has been translated from swift/test/Parse/effectful_properties.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class EffectfulPropertiesTests extends AstSwiftSrc2CpgSuite {

  "EffectfulPropertiesTests" should {

    "testEffectfulProperties1" ignore {
      val cpg = code("""
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
        |}""".stripMargin)
      ???
    }

    "testEffectfulProperties2" ignore {
      val cpg = code("""
        |struct X1 {
        |  subscript(_ i : Int) -> Int {
        |    get async {}
        |  }
        |}""".stripMargin)
      ???
    }

    "testEffectfulProperties3" ignore {
      val cpg = code("""
        |class X2 {
        |  subscript(_ i : Int) -> Int {
        |    get throws {}
        |  }
        |}""".stripMargin)
      ???
    }

    "testEffectfulProperties4" ignore {
      val cpg = code("""
        |struct X3 {
        |  subscript(_ i : Int) -> Int {
        |    get async throws {}
        |  }
        |}""".stripMargin)
      ???
    }

    "testEffectfulProperties5" ignore {
      val cpg = code("""
        |struct BadSubscript1 {
        |  subscript(_ i : Int) -> Int {
        |    get async throws {}
        |    set {}
        |  }
        |}""".stripMargin)
      ???
    }

    "testEffectfulProperties6" ignore {
      val cpg = code("""
        |struct BadSubscript2 {
        |  subscript(_ i : Int) -> Int {
        |    get throws {}
        |    set throws {}
        |  }
        |}""".stripMargin)
      ???
    }

    "testEffectfulProperties7" ignore {
      val cpg = code("""
        |struct S {
        |  var prop2 : Int {
        |    mutating get async throws { 0 }
        |    nonmutating set {}
        |  }
        |}
        |""".stripMargin)
      ???
    }

    "testEffectfulProperties8" ignore {
      val cpg = code("""
        |var prop3 : Bool {
        |  _read { yield prop3 }
        |  get throws { false }
        |  get async { true }
        |  get {}
        |}
        |""".stripMargin)
      ???
    }

    "testEffectfulProperties9" ignore {
      val cpg = code("""
        |enum E {
        |  private(set) var prop4 : Double {
        |    set {}
        |    get async throws { 1.1 }
        |    _modify { yield &prop4 }
        |  }
        |}
        |""".stripMargin)
      ???
    }

    "testEffectfulProperties10" ignore {
      val cpg = code("""
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
        |""".stripMargin)
      ???
    }

    "testEffectfulProperties14" ignore {
      val cpg = code("""
        |var bad3 : Int {
        |  _read async { yield 0 }
        |  set(theValue) async { }
        |}
        |""".stripMargin)
      ???
    }

  }

}
