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

}
