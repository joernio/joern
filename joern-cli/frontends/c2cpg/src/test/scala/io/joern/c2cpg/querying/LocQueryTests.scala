package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class LocQueryTests extends C2CpgSuite {

  private implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  private val cpg = code("""
   |int my_func(int param1) {
   |   int x = foo(param1);
   |}""".stripMargin)

  "should return loc for method" in {
    val locs = cpg.method.name("my_func").loc.l
    locs.size shouldBe 1

    val loc = locs.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "my_func"
    loc.lineNumber shouldBe Option(2)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "METHOD"
  }

  "should return loc for parameter" in {
    val locs = cpg.parameter.name("param1").loc.l
    locs.size shouldBe 1

    val loc = locs.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "param1"
    loc.lineNumber shouldBe Option(2)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "METHOD_PARAMETER_IN"

  }

  "should return loc for return parameter" in {
    val locs = cpg.method.name("my_func").methodReturn.loc.l
    locs.size shouldBe 1

    val loc = locs.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "$ret"
    loc.lineNumber shouldBe Option(2)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "METHOD_RETURN"

  }

  "should return loc for call" in {
    val locs = cpg.call.name("foo").loc.l
    locs.size shouldBe 1

    val loc = locs.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "foo(param1)"
    loc.lineNumber shouldBe Option(3)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "CALL"

  }

  "should return loc for identifier" in {
    val locs = cpg.identifier.name("x").loc.l
    locs.size shouldBe 1

    val loc = locs.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "x"
    loc.lineNumber shouldBe Option(3)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "IDENTIFIER"
  }

  "should return loc for local" in {
    val locs = cpg.local.name("x").loc.l
    locs.size shouldBe 1

    val loc = locs.head
    loc.methodFullName shouldBe "my_func"
    loc.methodShortName shouldBe "my_func"
    loc.symbol shouldBe "x"
    loc.lineNumber shouldBe Option(3)
    loc.filename should endWith(".c")
    loc.nodeLabel shouldBe "LOCAL"
  }

}
