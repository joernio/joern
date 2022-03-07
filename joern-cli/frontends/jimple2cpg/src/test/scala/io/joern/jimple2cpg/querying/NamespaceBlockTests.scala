package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class NamespaceBlockTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """
      |package foo.bar;
      |class A {
      | static void foo() {}
      |}
      |""".stripMargin

  "should contain two namespace blocks in total (<default>, foo.bar)" in {
    cpg.namespaceBlock.size shouldBe 2
  }

  "should contain correct namespace block for known file" in {
    val List(x) = cpg.namespaceBlock.filename(".*.class").l
    x.name shouldBe "bar"
    x.filename should not be ""
    x.fullName shouldBe s"foo.bar"
    x.order shouldBe 1
  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filename(".*.class").typeDecl.method.name.toSetMutable shouldBe Set("foo", "<init>")
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock.filename(".*.class").typeDecl.name.l shouldBe List("A")
  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filename(".*.class").namespace.name.l shouldBe List("bar")
  }

}
