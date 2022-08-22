package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class LombokTests extends JavaSrcCode2CpgFixture(runDelombok = true) {

  "source with lombok annotations should be successfully delomboked" in {
    val cpg = code(
      """
        |import lombok.Getter;
        |
        |public class Foo {
        |    @Getter private int value = 42;
        |}""".stripMargin,
      fileName = "Foo.java"
    )

    cpg.method.name("getValue").l match {
      case method :: Nil =>
        method.fullName shouldBe "Foo.getValue:int()"
        method.body.astChildren.size shouldBe 1

      case result => fail(s"Expected single getValue method but got $result")
    }
  }

  "source with lombok annotations should have correct type information" in {
    val cpg = code(
      """
		|import lombok.Getter;
		|
		|public class Foo {
		|  @Getter
		|  private String firstName;
		|
		|  public Foo() {
		|    firstName = "WALLY";
		|  }
		|}""".stripMargin,
      fileName = "Foo.java"
    ).moreCode(
      """
	    |public class Bar {
        |
        |  public void printObject(Object o) {
        |    System.out.println(o.toString());
        |  }
        |
        |  public void bar() {
        |    Foo f = new Foo();
        |    printObject(f.getFirstName());
        |  }
        |}
        |""".stripMargin,
      fileName = "Bar.java"
    )

    cpg.call.name("getFirstName").methodFullName.head shouldBe "Foo.getFirstName:java.lang.String()"
  }
}
