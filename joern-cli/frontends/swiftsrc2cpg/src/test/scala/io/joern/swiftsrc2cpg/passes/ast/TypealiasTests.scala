// This test file has been translated from swift/test/Parse/typealias.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class TypealiasTests extends AstSwiftSrc2CpgSuite {

  "TypealiasTests" should {

    "testTypealias2a1" in {
      val cpg = code("""
        |typealias IntPair = (Int, Int)
        |extension IntPair {
        |  var foo: String
        |}
        |""".stripMargin)
      val expectedTypeAliasFullName = "Test0.swift:<global>:<anon-class>0"
      val List(tupleTypeDecl)       = cpg.typeDecl.fullNameExact(expectedTypeAliasFullName).l
      tupleTypeDecl.name shouldBe "<anon-class>0"
      tupleTypeDecl.code shouldBe "(Int, Int)"

      val List(intPair) = cpg.typeDecl.nameExact("IntPair").l
      intPair.aliasTypeFullName shouldBe Option(expectedTypeAliasFullName)

      val List(intPairExt) = cpg.typeDecl.nameExact("IntPair<extension>").l
      intPairExt.member.code.l shouldBe List("foo")
    }

    "testTypealias2a2" in {
      val cpg = code("""
        |extension IntPair {
        |  var foo: String
        |}
        |typealias IntPair = (Int, Int)
        |""".stripMargin)
      val expectedTypeAliasFullName = "Test0.swift:<global>:<anon-class>0"
      val List(tupleTypeDecl)       = cpg.typeDecl.fullNameExact(expectedTypeAliasFullName).l
      tupleTypeDecl.name shouldBe "<anon-class>0"
      tupleTypeDecl.code shouldBe "(Int, Int)"

      val List(intPair) = cpg.typeDecl.nameExact("IntPair").l
      intPair.aliasTypeFullName shouldBe Option(expectedTypeAliasFullName)

      val List(intPairExt) = cpg.typeDecl.nameExact("IntPair<extension>").l
      intPairExt.member.code.l shouldBe List("foo")
    }

    "testTypealias2b" in {
      val cpg                       = code("typealias IntTriple = (Int, Int, Int)")
      val expectedTypeAliasFullName = "Test0.swift:<global>:<anon-class>0"
      val List(tupleTypeDecl)       = cpg.typeDecl.fullNameExact(expectedTypeAliasFullName).l
      tupleTypeDecl.name shouldBe "<anon-class>0"
      tupleTypeDecl.code shouldBe "(Int, Int, Int)"

      val List(intTriple) = cpg.typeDecl.nameExact("IntTriple").l
      intTriple.aliasTypeFullName shouldBe Option(expectedTypeAliasFullName)
    }

    "testTypealias3a" in {
      val cpg        = code("typealias Foo1 = Int")
      val List(foo1) = cpg.typeDecl.nameExact("Foo1").l
      foo1.aliasTypeFullName shouldBe Option("Int")
    }

    "testTypealias9" in {
      val cpg             = code("typealias Recovery5 = Int, Float")
      val List(recovery5) = cpg.typeDecl.nameExact("Recovery5").l
      recovery5.aliasTypeFullName shouldBe Option("Int")
    }

    "testTypealias11" in {
      val cpg      = code("typealias `switch` = Int")
      val List(sw) = cpg.typeDecl.nameExact("`switch`").l
      sw.aliasTypeFullName shouldBe Option("Int")
    }
  }

}
