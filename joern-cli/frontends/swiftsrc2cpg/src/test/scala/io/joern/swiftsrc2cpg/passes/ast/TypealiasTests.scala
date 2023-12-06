// This test file has been translated from swift/test/Parse/typealias.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TypealiasTests extends AbstractPassTest {

  "TypealiasTests" should {

    "testTypealias2a" ignore AstFixture("typealias IntPair = (Int, Int)") { cpg => ??? }

    "testTypealias2b" ignore AstFixture("typealias IntTriple = (Int, Int, Int)") { cpg => ??? }

    "testTypealias2d" ignore AstFixture("var fiveInts : FiveInts = ((4,2), (1,2,3))") { cpg => ??? }

    "testTypealias3a" ignore AstFixture("typealias Foo1 = Int") { cpg => ??? }

    "testTypealias9" ignore AstFixture("typealias Recovery5 = Int, Float") { cpg => ??? }

    "testTypealias11" ignore AstFixture("typealias `switch` = Int") { cpg => ??? }
  }

}
