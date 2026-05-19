// This test file has been translated from swift/test/Parse/generic_disambiguation.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class GenericDisambiguationTests extends SwiftSrc2CpgSuite {

  "GenericDisambiguationTests" should {

    "testGenericDisambiguation1" in {
      val cpg = code("""
        |struct A<B> {
        |  init(x:Int) {}
        |  static func c() {}
        |  struct C<D> {
        |    static func e() {}
        |  }
        |  struct F {}
        |}
        |struct B {}
        |struct D {}
        |""".stripMargin)
      // Nested generic struct A<B> with inner C<D> and F. We assert nesting-aware fullNames.
      cpg.typeDecl.fullName.l should contain allOf (
        "Test0.swift:<global>.A",
        "Test0.swift:<global>.A.C",
        "Test0.swift:<global>.A.F",
        "Test0.swift:<global>.B",
        "Test0.swift:<global>.D"
      )
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.A.init:(x:Swift.Int)->Test0.swift:<global>.A",
        "Test0.swift:<global>.A.c:()->ANY",
        "Test0.swift:<global>.A.C.e:()->ANY"
      )
    }

    "testGenericDisambiguation2" in {
      val cpg = code("""
        |protocol Runcible {}
        |protocol Fungible {}
        |""".stripMargin)
      cpg.typeDecl.fullName.l should contain allOf (
        "Test0.swift:<global>.Runcible",
        "Test0.swift:<global>.Fungible"
      )
    }

    "testGenericDisambiguation3" in {
      val cpg = code("""
        |func meta<T>(_ m: T.Type) {}
        |func meta2<T>(_ m: T.Type, _ x: Int) {}
        |""".stripMargin)
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.meta:(_:T)->ANY",
        "Test0.swift:<global>.meta2:(_:T,_:Swift.Int)->ANY"
      )
    }

    "testGenericDisambiguation4" in {
      val cpg           = code("func generic<T>(_ x: T) {}")
      val List(generic) = cpg.method.nameExact("generic").l
      generic.fullName shouldBe "Test0.swift:<global>.generic:(_:T)->ANY"
    }

    "testGenericDisambiguation5" in {
      // `var a, b, c, d : Int` — Swift's grouped declaration. The CPG types only `d` with
      // the trailing annotation; a, b, c get ANY since they have no individual annotation.
      val cpg = code("var a, b, c, d : Int")
      cpg.local.name.l shouldBe List("a", "b", "c", "d")
      cpg.local.typeFullName.l shouldBe List("ANY", "ANY", "ANY", "Swift.Int")
    }

    "testGenericDisambiguation9" in {
      val cpg            = code("A<B>(x: 0)")
      val List(callNode) = cpg.call.nameExact("A<B>").l
      callNode.code shouldBe "A<B>(x: 0)"
    }

    "testGenericDisambiguation10" in {
      val cpg            = code("meta(A<B>.self)")
      val List(metaCall) = cpg.call.nameExact("meta").l
      metaCall.code shouldBe "meta(A<B>.self)"
      cpg.call.nameExact("<operator>.fieldAccess").code.l should contain("A.self")
    }

    "testGenericDisambiguation11" in {
      val cpg             = code("meta2(A<B>.self, 0)")
      val List(meta2Call) = cpg.call.nameExact("meta2").l
      meta2Call.code shouldBe "meta2(A<B>.self, 0)"
      cpg.call.nameExact("<operator>.fieldAccess").code.l should contain("A.self")
    }

    "testGenericDisambiguation12" in {
      val cpg         = code("A<B>.C<D>.e()")
      val List(eCall) = cpg.call.nameExact("e").l
      eCall.code shouldBe "A<B>.C<D>.e()"
    }

    "testGenericDisambiguation13" in {
      val cpg                 = code("A<B>.C<D>(0)")
      val List(constructCall) = cpg.call.nameExact("A<B>.C<D>").l
      constructCall.code shouldBe "A<B>.C<D>(0)"
      cpg.call.nameExact("<operator>.fieldAccess").code.l should contain("A.C")
    }

    "testGenericDisambiguation14" in {
      val cpg = code("""
        |meta1(A<B>.C<D>.self)
        |meta2(A<B>.C<D>.self, 0)
        |""".stripMargin)
      cpg.call.nameExact("meta1").code.l shouldBe List("meta1(A<B>.C<D>.self)")
      cpg.call.nameExact("meta2").code.l shouldBe List("meta2(A<B>.C<D>.self, 0)")
      cpg.call.nameExact("<operator>.fieldAccess").code.l should contain("A.C.self")
    }

    "testGenericDisambiguation18" in {
      val cpg            = code("A<(B) throws -> D>(x: 0)")
      val List(callNode) = cpg.call.nameExact("A<(B) throws -> D>").l
      callNode.code shouldBe "A<(B) throws -> D>(x: 0)"
    }

  }

}
