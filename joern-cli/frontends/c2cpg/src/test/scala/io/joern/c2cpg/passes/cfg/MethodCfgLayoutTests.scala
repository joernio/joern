package io.joern.c2cpg.passes.cfg

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class MethodCfgLayoutTests extends C2CpgSuite {

  "CFG layout" should {
    val cpg = code("""
        |void method1() {
        |  int x = 1;
        |}
        |
        |void method2() {
        |  int x;
        |  int y;
        |  int z;
        |
        |  x = y + z;
        |}
        |""".stripMargin)

    "be correct for decl assignment in method1" in {
      val x = cpg.method.nameExact("method1").cfgNext.collectAll[Identifier].head
      x.name shouldBe "x"

      val one = x.cfgOut.collectAll[Literal].head
      one.code shouldBe "1"

      val call = one.cfgOut.collectAll[Call].head
      call.name shouldBe Operators.assignment

      val ret = call.cfgOut.collectAll[MethodReturn].head
      ret.code shouldBe "RET"
    }

    "be correct for nested expression in method2" in {
      val x = cpg.method.nameExact("method2").cfgNext.collectAll[Identifier].head
      x.name shouldBe "x"

      val y = x.cfgNext.collectAll[Identifier].head
      y.name shouldBe "y"

      val z = y.cfgNext.collectAll[Identifier].head
      z.name shouldBe "z"

      val call1 = z.cfgOut.collectAll[Call].head
      call1.name shouldBe Operators.addition

      val call2 = call1.cfgOut.collectAll[Call].head
      call2.name shouldBe Operators.assignment

      val ret = call2.cfgOut.collectAll[MethodReturn].head
      ret.code shouldBe "RET"
    }
  }

}
