// This test file has been translated from swift/test/Parse/generic_disambiguation.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class GenericDisambiguationTests extends AbstractPassTest {

  "GenericDisambiguationTests" should {

    "testGenericDisambiguation1" ignore AstFixture("""
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
        |""".stripMargin) { cpg => ??? }

    "testGenericDisambiguation2" ignore AstFixture("""
        |protocol Runcible {}
        |protocol Fungible {}
        |""".stripMargin) { cpg => ??? }

    "testGenericDisambiguation3" ignore AstFixture("""
        |func meta<T>(_ m: T.Type) {}
        |func meta2<T>(_ m: T.Type, _ x: Int) {}
        |""".stripMargin) { cpg => ??? }

    "testGenericDisambiguation4" ignore AstFixture("func generic<T>(_ x: T) {}") { cpg => ??? }

    "testGenericDisambiguation5" ignore AstFixture("var a, b, c, d : Int") { cpg => ??? }

    "testGenericDisambiguation9" ignore AstFixture("A<B>(x: 0)") { cpg => ??? }

    "testGenericDisambiguation10" ignore AstFixture("meta(A<B>.self)") { cpg => ??? }

    "testGenericDisambiguation11" ignore AstFixture("meta2(A<B>.self, 0)") { cpg => ??? }

    "testGenericDisambiguation12" ignore AstFixture("A<B>.C<D>.e()") { cpg => ??? }

    "testGenericDisambiguation13" ignore AstFixture("A<B>.C<D>(0)") { cpg => ??? }

    "testGenericDisambiguation14" ignore AstFixture("""
        |meta1(A<B>.C<D>.self)
        |meta2(A<B>.C<D>.self, 0)
        |""".stripMargin) { cpg => ??? }

    "testGenericDisambiguation18" ignore AstFixture("A<(B) throws -> D>(x: 0)") { cpg => ??? }

  }

}
