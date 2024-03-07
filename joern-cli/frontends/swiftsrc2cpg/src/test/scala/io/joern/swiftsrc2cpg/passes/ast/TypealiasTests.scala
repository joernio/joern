// This test file has been translated from swift/test/Parse/typealias.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TypealiasTests extends AbstractPassTest {

  "TypealiasTests" should {

    "testTypealias2a1" in AstFixture("""
        |typealias IntPair = (Int, Int)
        |extension IntPair {
        |  var foo: String
        |}
        |""".stripMargin) { cpg =>
      val List(intPair) = cpg.typeDecl.nameExact("IntPair").l
      intPair.aliasTypeFullName shouldBe Option("(Int, Int)")

      val List(intPairExt) = cpg.typeDecl.nameExact("IntPair<extension>").l
      intPairExt.member.code.l shouldBe List("foo")
    }

    "testTypealias2a2" in AstFixture("""
        |extension IntPair {
        |  var foo: String
        |}
        |typealias IntPair = (Int, Int)
        |""".stripMargin) { cpg =>
      val List(intPair) = cpg.typeDecl.nameExact("IntPair").l
      intPair.aliasTypeFullName shouldBe Option("(Int, Int)")

      val List(intPairExt) = cpg.typeDecl.nameExact("IntPair<extension>").l
      intPairExt.member.code.l shouldBe List("foo")
    }

    "testTypealias2b" in AstFixture("typealias IntTriple = (Int, Int, Int)") { cpg =>
      val List(intTriple) = cpg.typeDecl.nameExact("IntTriple").l
      intTriple.aliasTypeFullName shouldBe Option("(Int, Int, Int)")
    }

    "testTypealias3a" in AstFixture("typealias Foo1 = Int") { cpg =>
      val List(foo1) = cpg.typeDecl.nameExact("Foo1").l
      foo1.aliasTypeFullName shouldBe Option("Int")
    }

    "testTypealias9" in AstFixture("typealias Recovery5 = Int, Float") { cpg =>
      val List(recovery5) = cpg.typeDecl.nameExact("Recovery5").l
      recovery5.aliasTypeFullName shouldBe Option("Int")
    }

    "testTypealias11" in AstFixture("typealias `switch` = Int") { cpg =>
      val List(sw) = cpg.typeDecl.nameExact("`switch`").l
      sw.aliasTypeFullName shouldBe Option("Int")
    }
  }

}
