

// This test file has been translated from swift/test/Parse/result-builder.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ResultBuilderTests extends AbstractPassTest {
  "testResultBuilder1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // rdar://70158735
      """
    )
  }

  "testResultBuilder2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @resultBuilder
      struct A<T> {
        static func buildBlock(_ values: Int...) -> Int { return 0 }
      }
      """
    )
  }

  "testResultBuilder3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct B<T> {}
      """
    )
  }

  "testResultBuilder4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension B {
        @resultBuilder
        struct Generic<U> {
          static func buildBlock(_ values: Int...) -> Int { return 0 }
        }
        @resultBuilder
        struct NonGeneric {
          static func buildBlock(_ values: Int...) -> Int { return 0 }
        }
      }
      """
    )
  }

  "testResultBuilder5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @A<Float> var test0: Int {
        1
        2
        3
      }
      """
    )
  }

  "testResultBuilder6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @B<Float>.NonGeneric var test1: Int {
        1
        2
        3
      }
      """
    )
  }

  "testResultBuilder7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @B<Float>.Generic<Float> var test2: Int {
        1
        2
        3
      }
      """
    )
  }

}
