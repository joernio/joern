package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class VariadicGenericsTests extends SwiftSrc2CpgSuite {

  "VariadicGenericsTests" should {

    "testRequirement" in {
      val cpg               = code("func requirement<each T>() where each T: P {}")
      val List(requirement) = cpg.method.nameExact("requirement").l
      requirement.fullName shouldBe "Test0.swift:<global>.requirement:()->ANY"
    }

    "testPackElementExprSimple" in {
      val cpg = code("""
        |func tuplify<each T>(_ t: repeat each T) -> (repeat each T) {
        |  return (repeat each t)
        |}
        |func zip<each T, each U>(_ first: repeat each T, with second: repeat each U) -> (repeat (each T, each U)) {
        |  return (repeat (each first, each second))
        |}
        |func variadicMap<each T, each Result>(_ t: repeat each T, transform: repeat (each T) -> each Result) -> (repeat each Result) {
        |  return (repeat (each transform)(each t))
        |}
        |""".stripMargin)
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.tuplify:(_:repeat each T)->(repeat each T)",
        "Test0.swift:<global>.zip:(_:repeat each T,with:repeat each U)->(repeat (each T, each U))",
        "Test0.swift:<global>.variadicMap:(_:repeat each T,transform:repeat (each T) -> each Result)->(repeat each Result)"
      )
    }

    "testMetatype" in {
      val cpg = code("G<Int, repeat Array<each T>>.self")
      // `G<...>.self` is a metatype access — emerges as a fieldAccess on `G`.
      cpg.call.nameExact("<operator>.fieldAccess").code.l shouldBe List("G.self")
    }

    "testParameterPacks1" in {
      val cpg      = code("func f1<each T>() -> repeat each T {}")
      val List(f1) = cpg.method.nameExact("f1").l
      f1.fullName shouldBe "Test0.swift:<global>.f1:()->repeat each T"
    }

    "testParameterPacks2" in {
      val cpg      = code("func f2<each T>() -> (repeat each T) {}")
      val List(f2) = cpg.method.nameExact("f2").l
      f2.fullName shouldBe "Test0.swift:<global>.f2:()->(repeat each T)"
    }

    "testParameterPacks3" in {
      val cpg      = code("func f3<each T>() -> G<repeat each T> {}")
      val List(f3) = cpg.method.nameExact("f3").l
      // Generic type arguments are stripped from method return-type names.
      f3.fullName shouldBe "Test0.swift:<global>.f3:()->G"
    }

    "testParameterPacks5" in {
      val cpg         = code("typealias Alias<each T> = (repeat each T)")
      val List(alias) = cpg.typeDecl.nameExact("Alias").l
      alias.fullName shouldBe "Test0.swift:<global>.Alias"
      alias.aliasTypeFullName shouldBe Some("Test0.swift:<global>.<tuple-type>0")
    }

    "testParameterPacks6" in {
      val cpg     = code("struct S<each T> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks7" in {
      val cpg     = code("struct S<T, each U> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks8" in {
      val cpg     = code("struct S<each T, U> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks9" in {
      val cpg     = code("struct S<each T:P, U> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks10" in {
      val cpg     = code("struct S<each T :P, U> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks11" in {
      val cpg     = code("struct S<each T: P> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks12" in {
      val cpg     = code("struct S<each T : P> {}")
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.fullName shouldBe "Test0.swift:<global>.S"
    }

    "testParameterPacks13" in {
      val cpg       = code("func foo<each T>(_ x: repeat each T) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(_:repeat each T)->ANY"
    }

    "testParameterPacks14" in {
      val cpg       = code("func foo<each T:P>(_ x: repeat each T) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(_:repeat each T)->ANY"
    }

    "testParameterPacks15" in {
      val cpg       = code("func foo<each T :P>(_ x: repeat each T) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(_:repeat each T)->ANY"
    }

    "testParameterPacks16" in {
      val cpg       = code("func foo<each T : P>(_ x: repeat each T) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(_:repeat each T)->ANY"
    }

    "testParameterPacks17" in {
      val cpg       = code("func foo<each T: P>(_ x: repeat each T) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(_:repeat each T)->ANY"
    }

    "testParameterPacks18" in {
      val cpg       = code("func foo<T, U, each V>(x: T, y: U, z: repeat each V) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(x:T,y:U,z:repeat each V)->ANY"
    }

    "testParameterPacks19" in {
      val cpg       = code("func foo<T, each U, V>(x: T, y: repeat each U, z: V) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(x:T,y:repeat each U,z:V)->ANY"
    }

    "testParameterPacks20" in {
      val cpg       = code("func foo<each T, each U, each V>(x: repeat each T, y: repeat each U, z: repeat each V) {}")
      val List(foo) = cpg.method.nameExact("foo").l
      foo.fullName shouldBe "Test0.swift:<global>.foo:(x:repeat each T,y:repeat each U,z:repeat each V)->ANY"
    }

    "testParameterPacks21" in {
      val cpg = code("""
        |enum E<each T> {
        |  case f1(_: repeat each T)
        |}
        |""".stripMargin)
      val List(e) = cpg.typeDecl.nameExact("E").l
      e.fullName shouldBe "Test0.swift:<global>.E"
      val List(f1Member) = e.member.nameExact("f1").l
      f1Member.typeFullName shouldBe "ANY"
    }

    "testParameterPacks22" in {
      val cpg = code("""
        |enum E<each T> {
        |  case f2(_: G<repeat each T>)
        |}
        |""".stripMargin)
      val List(e) = cpg.typeDecl.nameExact("E").l
      e.fullName shouldBe "Test0.swift:<global>.E"
    }

    "testParameterPacks23" in {
      val cpg = code("""
        |enum E<each T> {
        |  var x: repeat each T { fatalError() }
        |}
        |""".stripMargin)
      val List(xMethod) = cpg.method.nameExact("x").l
      xMethod.fullName shouldBe "Test0.swift:<global>.E.x:repeat each T"
    }

    "testParameterPacks24" in {
      val cpg = code("""
        |enum E<each T> {
        |  var x: (repeat each T) { fatalError() }
        |}
        |""".stripMargin)
      val List(xMethod) = cpg.method.nameExact("x").l
      xMethod.fullName shouldBe "Test0.swift:<global>.E.x:(repeat each T)"
    }

    "testParameterPacks25" in {
      val cpg = code("""
        |enum E<each T> {
        |  subscript(_: repeat each T) -> Int { fatalError() }
        |}
        |""".stripMargin)
      val List(subscriptMethod) = cpg.method.nameExact("subscript").l
      subscriptMethod.fullName shouldBe "Test0.swift:<global>.E.subscript:(_:repeat each T)->Swift.Int"
    }

    "testParameterPacks26" in {
      val cpg = code("""
        |enum E<each T> {
        |  subscript() -> repeat each T { fatalError() }
        |}
        |""".stripMargin)
      val List(subscriptMethod) = cpg.method.nameExact("subscript").l
      subscriptMethod.fullName shouldBe "Test0.swift:<global>.E.subscript:()->repeat each T"
    }

    "testParameterPacks27" in {
      val cpg = code("""
        |enum E<each T> {
        |  subscript() -> (repeat each T) { fatalError() }
        |}
        |""".stripMargin)
      val List(subscriptMethod) = cpg.method.nameExact("subscript").l
      subscriptMethod.fullName shouldBe "Test0.swift:<global>.E.subscript:()->(repeat each T)"
    }

    "testVariadicTypes" in {
      val cpg = code("""
        |let _: G< > = G()
        |let _: G<repeat each T> = G()
        |let _: G<Int, repeat each T> = G()
        |let _ = G< >.self
        |let _ = G<repeat each T>.self
        |let _ = G<Int, repeat each T>.self
        |""".stripMargin)
      // Six wildcard locals: first three carry the explicit `G<...>` annotation, last three
      // are inferred (ANY) since the right-hand side is the metatype access.
      val wildcards = cpg.local.name.filter(_.startsWith("<wildcard>")).l
      wildcards shouldBe List("<wildcard>0", "<wildcard>1", "<wildcard>2", "<wildcard>3", "<wildcard>4", "<wildcard>5")
      cpg.local.nameExact("<wildcard>0", "<wildcard>1", "<wildcard>2").typeFullName.l shouldBe List("G", "G", "G")
      cpg.local.nameExact("<wildcard>3", "<wildcard>4", "<wildcard>5").typeFullName.l shouldBe List("ANY", "ANY", "ANY")
    }

    "testMissingCommaInType" in {
      val cpg = code("""
        |var foo1: (Int)
        |var foo2: (Int, Int)
        |""".stripMargin)
      // Tuple type annotations are recorded verbatim on the local.
      cpg.local.nameExact("foo1").typeFullName.l shouldBe List("(Int)")
      cpg.local.nameExact("foo2").typeFullName.l shouldBe List("(Int, Int)")
    }

  }

}
