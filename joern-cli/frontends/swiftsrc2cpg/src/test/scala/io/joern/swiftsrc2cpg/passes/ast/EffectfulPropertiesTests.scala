// This test file has been translated from swift/test/Parse/effectful_properties.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class EffectfulPropertiesTests extends SwiftSrc2CpgSuite {

  "EffectfulPropertiesTests" should {

    "testEffectfulProperties1" in {
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
      val List(structDecl) = cpg.typeDecl.nameExact("MyProps").l
      structDecl.member.name.l shouldBe List("prop1", "prop2", "prop3", "prop1mut", "prop2mut", "prop3mut")
      structDecl.member.typeFullName.l.distinct shouldBe List("Swift.Int")
      structDecl.method.name.l shouldBe List(
        "init",
        "prop1.getter",
        "prop2.getter",
        "prop3.getter",
        "prop1mut.getter",
        "prop2mut.getter",
        "prop3mut.getter"
      )
      structDecl.method.nameExact("prop1.getter").fullName.head shouldBe
        "Test0.swift:<global>.MyProps.prop1.getter:Swift.Int"
    }

    "testEffectfulProperties2" in {
      val cpg = code("""
        |struct X1 {
        |  subscript(_ i : Int) -> Int {
        |    get async {}
        |  }
        |}""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("X1").l
      structDecl.member.l shouldBe empty
      structDecl.method.name.l shouldBe List("init", "getter")
      structDecl.method.nameExact("getter").fullName.head shouldBe
        "Test0.swift:<global>.X1.subscript:(_:Swift.Int).getter:Swift.Int"
    }

    "testEffectfulProperties3" in {
      val cpg = code("""
        |class X2 {
        |  subscript(_ i : Int) -> Int {
        |    get throws {}
        |  }
        |}""".stripMargin)
      val List(classDecl) = cpg.typeDecl.nameExact("X2").l
      classDecl.method.name.l shouldBe List("init", "getter")
      classDecl.method.nameExact("getter").fullName.head shouldBe
        "Test0.swift:<global>.X2.subscript:(_:Swift.Int).getter:Swift.Int"
    }

    "testEffectfulProperties4" in {
      val cpg = code("""
        |struct X3 {
        |  subscript(_ i : Int) -> Int {
        |    get async throws {}
        |  }
        |}""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("X3").l
      structDecl.method.name.l shouldBe List("init", "getter")
    }

    "testEffectfulProperties5" in {
      val cpg = code("""
        |struct BadSubscript1 {
        |  subscript(_ i : Int) -> Int {
        |    get async throws {}
        |    set {}
        |  }
        |}""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("BadSubscript1").l
      structDecl.method.name.l shouldBe List("init", "getter", "setter")
      structDecl.method.nameExact("setter").fullName.head shouldBe
        "Test0.swift:<global>.BadSubscript1.subscript:(_:Swift.Int).setter:Swift.Int"
    }

    "testEffectfulProperties6" in {
      val cpg = code("""
        |struct BadSubscript2 {
        |  subscript(_ i : Int) -> Int {
        |    get throws {}
        |    set throws {}
        |  }
        |}""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("BadSubscript2").l
      structDecl.method.name.l shouldBe List("init", "getter", "setter")
    }

    "testEffectfulProperties7" in {
      val cpg = code("""
        |struct S {
        |  var prop2 : Int {
        |    mutating get async throws { 0 }
        |    nonmutating set {}
        |  }
        |}
        |""".stripMargin)
      val List(structDecl) = cpg.typeDecl.nameExact("S").l
      structDecl.member.name.l shouldBe List("prop2")
      structDecl.method.name.l shouldBe List("init", "prop2.getter", "prop2.setter")
    }

    "testEffectfulProperties8" in {
      val cpg = code("""
        |var prop3 : Bool {
        |  _read { yield prop3 }
        |  get throws { false }
        |  get async { true }
        |  get {}
        |}
        |""".stripMargin)
      // Top-level variable with computed accessors becomes one method per accessor at file scope.
      cpg.method.nameExact("prop3._read").fullName.head shouldBe "Test0.swift:<global>.prop3._read:Swift.Bool"
      cpg.method.nameExact("prop3.getter").fullName.l shouldBe List(
        "Test0.swift:<global>.prop3.getter:Swift.Bool",
        "Test0.swift:<global>.prop3.getter<duplicate>0:Swift.Bool",
        "Test0.swift:<global>.prop3.getter<duplicate>1:Swift.Bool"
      )
    }

    "testEffectfulProperties9" in {
      val cpg = code("""
        |enum E {
        |  private(set) var prop4 : Double {
        |    set {}
        |    get async throws { 1.1 }
        |    _modify { yield &prop4 }
        |  }
        |}
        |""".stripMargin)
      val List(enumDecl) = cpg.typeDecl.nameExact("E").l
      enumDecl.member.name.l shouldBe List("prop4")
      enumDecl.method.name.l shouldBe List("init", "prop4.setter", "prop4.getter", "prop4._modify")
    }

    "testEffectfulProperties10" in {
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
      val List(protoDecl) = cpg.typeDecl.nameExact("P").l
      protoDecl.member.name.l shouldBe List("prop1", "prop2", "prop3", "prop4", "prop5", "prop6", "prop7")
      protoDecl.member.typeFullName.l.distinct shouldBe List("T")
      protoDecl.method.name.l should contain allOf ("prop1.getter", "prop2.setter", "prop7.setter")
    }

    "testEffectfulProperties14" in {
      val cpg = code("""
        |var bad3 : Int {
        |  _read async { yield 0 }
        |  set(theValue) async { }
        |}
        |""".stripMargin)
      cpg.method.nameExact("bad3._read").fullName.head shouldBe "Test0.swift:<global>.bad3._read:Swift.Int"
      cpg.method.nameExact("bad3.setter").fullName.head shouldBe "Test0.swift:<global>.bad3.setter:Swift.Int"
    }

  }

}
