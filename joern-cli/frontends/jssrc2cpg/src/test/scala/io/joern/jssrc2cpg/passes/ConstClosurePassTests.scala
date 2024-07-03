package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class ConstClosurePassTests extends DataFlowCodeToCpgSuite {

  "object properties assigned to closures" should {
    val cpg = code("""
        |var myObject = {
        |  prop1: "a",
        |  prop2: "b",
        |
        |  // Function 1
        |  sayHello: function () {
        |    console.log("sayHello");
        |  },
        |
        |  // Function 2
        |  sayWorld: function () {
        |    console.log("sayWorld");
        |  },
        |
        |  // Function 3
        |  sayHelloWorld: function () {
        |    console.log("sayHelloWorld");
        |  }
        |};
        |myObject.sayHello()
        |myObject.sayWorld()
        |myObject.sayHelloWorld()
        |""".stripMargin)

    "should return the methods via `cpg.method`" in {
      val List(sayHello) = cpg.method.name("sayHello").l
      sayHello.fullName.endsWith("program:sayHello") shouldBe true
      val List(sayHelloCall) = cpg.call("sayHello").l
      sayHelloCall.methodFullName.endsWith("program:sayHello") shouldBe true

      val List(sayWorld) = cpg.method.name("sayWorld").l
      sayWorld.fullName.endsWith("program:sayWorld") shouldBe true
      val List(sayWorldCall) = cpg.call("sayWorld").l
      sayWorldCall.methodFullName.endsWith("program:sayWorld") shouldBe true

      val List(sayHelloWorld) = cpg.method.name("sayHelloWorld").l
      sayHelloWorld.fullName.endsWith("program:sayHelloWorld") shouldBe true
      val List(sayHelloWorldCall) = cpg.call("sayHelloWorld").l
      sayHelloWorldCall.methodFullName.endsWith("program:sayHelloWorld") shouldBe true
    }
  }

  "should return method `foo` via `cpg.method`" in {
    val cpg = code("""
        |const foo = (x,y) => { return x + y; }
        |const bar = (x,y) => { return x - y; }
        |""".stripMargin)

    val List(foo) = cpg.method.name("foo").l
    foo.name shouldBe "foo"
    foo.fullName.endsWith("program:foo") shouldBe true

    val List(bar) = cpg.method.name("bar").l
    bar.name shouldBe "bar"
    bar.fullName.endsWith("program:bar") shouldBe true
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
