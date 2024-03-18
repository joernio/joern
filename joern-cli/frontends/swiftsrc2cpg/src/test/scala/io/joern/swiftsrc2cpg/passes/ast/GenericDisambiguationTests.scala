// This test file has been translated from swift/test/Parse/generic_disambiguation.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class GenericDisambiguationTests extends AstSwiftSrc2CpgSuite {

  "GenericDisambiguationTests" should {

    "testGenericDisambiguation1" ignore {
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
      ???
    }

    "testGenericDisambiguation2" ignore {
      val cpg = code("""
        |protocol Runcible {}
        |protocol Fungible {}
        |""".stripMargin)
      ???
    }

    "testGenericDisambiguation3" ignore {
      val cpg = code("""
        |func meta<T>(_ m: T.Type) {}
        |func meta2<T>(_ m: T.Type, _ x: Int) {}
        |""".stripMargin)
      ???
    }

    "testGenericDisambiguation4" ignore {
      val cpg = code("func generic<T>(_ x: T) {}")
      ???
    }

    "testGenericDisambiguation5" ignore {
      val cpg = code("var a, b, c, d : Int")
      ???
    }

    "testGenericDisambiguation9" ignore {
      val cpg = code("A<B>(x: 0)")
      ???
    }

    "testGenericDisambiguation10" ignore {
      val cpg = code("meta(A<B>.self)")
      ???
    }

    "testGenericDisambiguation11" ignore {
      val cpg = code("meta2(A<B>.self, 0)")
      ???
    }

    "testGenericDisambiguation12" ignore {
      val cpg = code("A<B>.C<D>.e()")
      ???
    }

    "testGenericDisambiguation13" ignore {
      val cpg = code("A<B>.C<D>(0)")
      ???
    }

    "testGenericDisambiguation14" ignore {
      val cpg = code("""
        |meta1(A<B>.C<D>.self)
        |meta2(A<B>.C<D>.self, 0)
        |""".stripMargin)
      ???
    }

    "testGenericDisambiguation18" ignore {
      val cpg = code("A<(B) throws -> D>(x: 0)")
      ???
    }

  }

}
