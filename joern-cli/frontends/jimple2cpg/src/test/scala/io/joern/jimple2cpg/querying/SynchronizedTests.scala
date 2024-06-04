package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SynchronizedTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      |class Foo {
      |  public static synchronized String foo(String s) {
      |    return s;
      |  }
      |
      |  public String bar(String s) {
      |    synchronized (this) {
      |      s += "A";
      |    }
      |    return s;
      |  }
      |
      |}
      |""".stripMargin).cpg

  "it should process a synchronized method the same as a non-synchronized method" in {
    val List(method: Method) = cpg.method.name("foo").l

    method.astChildren.size shouldBe 7
    val List("STATIC", "PUBLIC", "SYNCHRONIZED") = method.modifier.map(_.modifierType).l: @unchecked
    val List(param)                              = method.parameter.l
    val body                                     = method.block
    param.code shouldBe "java.lang.String s"
    body.astChildren.head shouldBe a[Return]
  }

  "it should create a enter/exit monitor nodes" in {
    val List(method: Method) = cpg.method.name("bar").l
    // 'l2' aliases 'this' so there is never an 'entermonitor l2'
    val List(enterThis, exit1, exit2) = method.ast.collectAll[Unknown].filter(_.code.contains("monitor")).l

    enterThis.code shouldBe "entermonitor this"
    enterThis.lineNumber shouldBe Some(8)
    enterThis.columnNumber shouldBe None

    exit1.code shouldBe "exitmonitor l2"
    exit1.lineNumber shouldBe Some(10)
    exit1.columnNumber shouldBe None

    exit2.code shouldBe "exitmonitor l2"
    exit2.lineNumber shouldBe Some(11)
    exit2.columnNumber shouldBe None
  }
}
