

// This test file has been translated from swift/test/Parse/generic_disambiguation.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class GenericDisambiguationTests extends AbstractPassTest {
  "testGenericDisambiguation1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A<B> {
        init(x:Int) {}
        static func c() {}
        struct C<D> {
          static func e() {}
        }
        struct F {}
      }
      struct B {}
      struct D {}
      """
    )
  }

  "testGenericDisambiguation2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol Runcible {}
      protocol Fungible {}
      """
    )
  }

  "testGenericDisambiguation3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func meta<T>(_ m: T.Type) {}
      func meta2<T>(_ m: T.Type, _ x: Int) {}
      """
    )
  }

  "testGenericDisambiguation4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func generic<T>(_ x: T) {}
      """
    )
  }

  "testGenericDisambiguation5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var a, b, c, d : Int
      """
    )
  }

  "testGenericDisambiguation6a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = a < b
      """
    )
  }

  "testGenericDisambiguation6b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = (a < b, c > d)
      """
    )
  }

  "testGenericDisambiguation6c" ignore AstFixture("") { cpg =>
    // Parses as generic because of lparen after '>'
    assertParse(
      """
      (a < b, c > (d))
      """,
      substructure: GenericArgumentListSyntax([
        GenericArgumentSyntax(
          argument: IdentifierTypeSyntax(name: .identifier("b")),
          trailingComma: .commaToken()
        ),
        GenericArgumentSyntax(
          argument: IdentifierTypeSyntax(name: .identifier("c"))
        ),
      ])
    )
  }

  "testGenericDisambiguation6d" ignore AstFixture("") { cpg =>
    // Parses as generic because of lparen after '>'
    assertParse(
      """
      (a<b, c>(d))
      """,
      substructure: GenericArgumentListSyntax([
        GenericArgumentSyntax(
          argument: IdentifierTypeSyntax(name: .identifier("b")),
          trailingComma: .commaToken()
        ),
        GenericArgumentSyntax(
          argument: IdentifierTypeSyntax(name: .identifier("c"))
        ),
      ])
    )
  }

  "testGenericDisambiguation6e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = a>(b)
      """
    )
  }

  "testGenericDisambiguation6f" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = a > (b)
      """
    )
  }

  "testGenericDisambiguation7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      generic<Int>(0)
      """
    )
  }

  "testGenericDisambiguation8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<B>.c()
      A<A<B>>.c()
      A<A<B>.F>.c()
      A<(A<B>) -> B>.c()
      A<[[Int]]>.c()
      A<[[A<B>]]>.c()
      A<(Int, UnicodeScalar)>.c()
      A<(a:Int, b:UnicodeScalar)>.c()
      A<Runcible & Fungible>.c()
      A<@convention(c) () -> Int32>.c()
      A<(@autoclosure @escaping () -> Int, Int) -> Void>.c()
      _ = [@convention(block) ()  -> Int]().count
      _ = [String: (@escaping (A<B>) -> Int) -> Void]().keys
      """
    )
  }

  "testGenericDisambiguation9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<B>(x: 0)
      """
    )
  }

  "testGenericDisambiguation10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      meta(A<B>.self)
      """
    )
  }

  "testGenericDisambiguation11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      meta2(A<B>.self, 0)
      """
    )
  }

  "testGenericDisambiguation12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<B>.C<D>.e()
      """
    )
  }

  "testGenericDisambiguation13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<B>.C<D>(0)
      """
    )
  }

  "testGenericDisambiguation14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      meta(A<B>.C<D>.self)
      meta2(A<B>.C<D>.self, 0)
      """
    )
  }

  "testGenericDisambiguation15" ignore AstFixture("") { cpg =>
    // TODO: parse empty <> list
    assertParse(
      """
      A<>.c()
      """
    )
  }

  "testGenericDisambiguation16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<B, D>.c()
      """
    )
  }

  "testGenericDisambiguation17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<B?>(x: 0) // parses as type
      _ = a < b ? c : d
      """
    )
  }

  "testGenericDisambiguation18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      A<(B) throws -> D>(x: 0)
      """
    )
  }

}
