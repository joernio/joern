

// This test file has been translated from swift/test/Parse/foreach_async.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ForeachAsyncTests extends AbstractPassTest {
  "testForeachAsync1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      import _Concurrency
      """
    )
  }

  "testForeachAsync2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct AsyncRange<Bound: Comparable & Strideable>: AsyncSequence, AsyncIteratorProtocol where Bound.Stride : SignedInteger {
        var range: Range<Bound>.Iterator
        typealias Element = Bound
        mutating func next() async -> Element? { return range.next() }
        func cancel() { }
        func makeAsyncIterator() -> Self { return self }
      }
      """
    )
  }

  "testForeachAsync3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct AsyncIntRange<Int> : AsyncSequence, AsyncIteratorProtocol {
        typealias Element = (Int, Int)
        func next() async -> (Int, Int)? {}
        func cancel() { }
        typealias AsyncIterator = AsyncIntRange<Int>
        func makeAsyncIterator() -> AsyncIntRange<Int> { return self }
      }
      """
    )
  }

  "testForeachAsync4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        var sum = 0
        // Simple foreach loop, using the variable in the body
        for await i in r {
          sum = sum + i
        }
      }
      """
    )
  }

  // Check scoping of variable introduced with foreach loop
  // For-each loops with two variables and varying degrees of typedness
  "testForeachAsync5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        var sum = 0
        for await (i, j) in iir {
          sum = sum + i + j
        }
      }
      """
    )

    assertParse(
      """
      func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        var sum = 0
        for await (i, j) in iir {
          sum = sum + i + j
        }
      }
      """
    )

    assertParse(
      """
      func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        var sum = 0
        for await (i, j) : (Int, Int) in iir {
          sum = sum + i + j
        }
      }
      """
    )
  }

  // Parse errors
  "testForeachAsync6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        var sum = 0
        for await i 1️⃣r {
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'in' in 'for' statement", fixIts: ["insert 'in'"])
      ],
      fixedSource: """
        func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
          var sum = 0
          for await i in r {
          }
        }
        """
    )

    assertParse(
      """
      func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        var sum = 0
        for await i in r 1️⃣sum = sum + i;2️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '{' in 'for' statement", fixIts: ["insert '{'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected '}' to end 'for' statement", fixIts: ["insert '}'"]),
      ],
      fixedSource: """
        func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
          var sum = 0
          for await i in r { sum = sum + i;
        }
        }
        """
    )
  }
}
