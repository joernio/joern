package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language._

class ObjectInstantiationTests extends JavaSrcCodeToCpgFixture {
  override val code: String =
    """
      |class Bar {
      |  public Bar(int x) {}
      |}
      |
      |class Test {
      |  public void foo() {
      |    Foo f = new Foo(12);
      |  }
      |
      |  public void bar() {
      |    Bar b = new Bar(55);
      |  }
      |}
      |""".stripMargin

  "should create an AST for object instantiations where the class isn't known" in {
    val assignment: Call = cpg.method(".*foo.*").assignments.l match {
      case List(assignment: Call) => assignment
      case res =>
        fail(s"Error extracting assignment. Expected `List(a: Call)` but got `$res`")
    }

    val (assignee: Identifier, initializer: Call) = assignment.argument.l match {
      case List(assignee: Identifier, initializer: Call) => (assignee, initializer)
      case res =>
        fail(s"Error extracting assign args. Expected `List(assignee: Identifier, initializer: Call)` but got $res")
    }

    withClue("assignee should be created correctly") {
      assignee.name shouldBe "f"
      assignee.typeFullName shouldBe "<unresolved>.Foo"
    }

    withClue("initializer should be created correctly") {
      initializer.name shouldBe "<constructor>.Foo"
      val List(arg: Literal) = initializer.argument.l
      arg.code shouldBe "12"
    }
  }

  "should create an AST for object instantiations where the class is known" in {
    val assignment: Call = cpg.method(".*bar.*").assignments.l match {
      case List(assignment: Call) => assignment
      case res =>
        fail(s"Error extracting assignment. Expected `List(a: Call)` but got `$res`")
    }

    val (assignee: Identifier, initializer: Call) = assignment.argument.l match {
      case List(assignee: Identifier, initializer: Call) => (assignee, initializer)
      case res =>
        fail(s"Error extracting assign args. Expected `List(assignee: Identifier, initializer: Call)` but got $res")
    }

    withClue("assignee should be created correctly") {
      assignee.name shouldBe "b"
      assignee.typeFullName shouldBe "Bar"
    }

    withClue("initializer should be created correctly") {
      initializer.name shouldBe "<constructor>.Bar"
      val List(arg: Literal) = initializer.argument.l
      arg.code shouldBe "55"
    }
  }
}
