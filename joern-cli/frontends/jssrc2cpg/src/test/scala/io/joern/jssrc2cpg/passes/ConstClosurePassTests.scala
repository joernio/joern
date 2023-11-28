package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class ConstClosurePassTests extends DataFlowCodeToCpgSuite {

  "should return method `foo` via `cpg.method`" in {
    val cpg = code("const foo = (x,y) => { return x + y; }")

    val List(m) = cpg.method.name("foo").l
    m.name shouldBe "foo"
    m.fullName.endsWith("program:foo") shouldBe true
  }

  "should return method `export.foo` via `cpg.method`" in {
    val cpg = code("""
        |exports.foo = (function() {
        |	var count = 0;
        |	return function() {
        |		count++;
        |		return count;
        |	};
        |})();
        |
        |this.foo();
        |""".stripMargin)

    val List(m) = cpg.method.name("foo").l
    m.name shouldBe "foo"
    m.fullName.endsWith("program:foo") shouldBe true
  }

  "mutable variables assigned to closures" should {
    val cpg = code("""
        |var foo = function() {};
        |foo();
        |
        |var bar = function() {};
        |bar();
        |bar = 2;
        |""".stripMargin)

    "be treated as a constant if not reassigned in the CPG" in {
      val List(foo) = cpg.method.name("foo").l
      foo.name shouldBe "foo"
      foo.fullName.endsWith("program:foo") shouldBe true
      val List(fooCall) = cpg.call("foo").l
      fooCall.methodFullName.endsWith("program:foo") shouldBe true
    }

    "keep the identifier assigned to the anonymous name if it is reassigned later" in {
      val List(bar) = cpg.method.name("<lambda>1").l
      bar.name shouldBe "<lambda>1"
      bar.fullName.endsWith("program:<lambda>1") shouldBe true
      val List(barCall) = cpg.call("bar").l
      barCall.methodFullName.endsWith("program:<lambda>1") shouldBe true
    }
  }

}
