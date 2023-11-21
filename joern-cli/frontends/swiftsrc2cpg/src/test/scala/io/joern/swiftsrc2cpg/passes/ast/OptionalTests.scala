// This test file has been translated from swift/test/Parse/optional.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class OptionalTests extends AbstractPassTest {

  "OptionalTests" should {

    "testOptional3a" ignore AstFixture("var c = a?") { cpg => ??? }

    "testOptional3b" ignore AstFixture("var d : ()? = a?.foo()") { cpg => ??? }

    "testOptional4" ignore AstFixture("""
        |var e : (() -> A)?
        |var f = e?()
        |""".stripMargin) { cpg => ??? }

    "testOptional5" ignore AstFixture("""
        |struct B<T> {}
        |var g = B<A?>()
        |""".stripMargin) { cpg => ??? }
  }

}
