package io.joern.csharpsrc2cpg.querying.dataflow

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class OperatorDataflowTests extends CSharpCode2CpgFixture(withDataFlow = true) {
  "simple operator dataflows" should {
    "be reachable (case 1)" in {
      val cpg = code(basicBoilerplate("""
          |int a = 0;
          |int b = 1;
          |int c = a + (b+3);
          |int d = c;
          |""".stripMargin))

      val src  = cpg.identifier.nameExact("a").l;
      val sink = cpg.identifier.nameExact("d").l
      sink.reachableBy(src).size shouldBe 2
      sink.reachableBy(cpg.literal("3")).size shouldBe 1
    }

    "be reachable (case 2)" in {
      val cpg = code(basicBoilerplate("""
          |int a = 1;
          |int b = 10;
          |
          |val c = a + b;
          |c = a - b;
          |c = a * b;
          |c = a / b;
          |c = a % b;
          |
          |c = a == b;
          |c = a > b;
          |c = a < b;
          |c = a >= b;
          |c = a <= b;
          |
          |c = a && b;
          |c = a || b;
          |
          |c = a & b;
          |c = a | b;
          |c = a ^ b;
          |
          |int d = c;
          |""".stripMargin))

      val src  = cpg.identifier.nameExact("a").l;
      val sink = cpg.identifier.nameExact("d").l
      sink.reachableBy(src).size shouldBe 16
    }

    "be reachable (case 3)" in {
      val cpg = code(basicBoilerplate("""
          |val a = 1;
          |val b = 10;
          |
          |a += b;
          |a -= b;
          |a *= b;
          |a /= b;
          |a %= b;
          |a &= b;
          |a |= b;
          |a >>= b;
          |a <<= b;
          |a ^= b;
          |
          |val c = a;
          |""".stripMargin))

      val src  = cpg.identifier.nameExact("a").l;
      val sink = cpg.identifier.nameExact("c").l
      sink.reachableBy(src).size shouldBe 12
    }
  }

}
