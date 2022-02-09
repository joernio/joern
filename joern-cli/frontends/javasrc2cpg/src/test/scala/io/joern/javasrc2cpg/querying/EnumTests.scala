package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._

class EnumTests extends JavaSrcCodeToCpgFixture {
  override val code: String =
    """
      |public enum FuzzyBool {
      |  TRUE,
      |  FALSE,
      |  MAYBE
      |}
      |
      |public enum Color {
      |  RED("Red"),
      |  BLUE("Blue");
      |
      |  public final String label;
      |
      |  private Color(String label) {
      |    this.label = label;
      |  }
      |}
      |""".stripMargin

  "it should parse a basic enum without values" in {
    cpg.typeDecl.name(".*FuzzyBool.*").nonEmpty shouldBe true
    cpg.typeDecl.name(".*FuzzyBool.*").member.size shouldBe 3
    val List(t, f, m) = cpg.typeDecl.name(".*FuzzyBool.*").member.l

    t.order shouldBe 1
    t.lineNumber shouldBe Some(3)
    t.columnNumber shouldBe Some(3)
    t.typeFullName shouldBe "FuzzyBool"
    t.name shouldBe "TRUE"
    t.code shouldBe "TRUE"

    f.order shouldBe 2
    f.lineNumber shouldBe Some(4)
    f.columnNumber shouldBe Some(3)
    f.typeFullName shouldBe "FuzzyBool"
    f.name shouldBe "FALSE"
    f.code shouldBe "FALSE"

    m.order shouldBe 3
    m.lineNumber shouldBe Some(5)
    m.columnNumber shouldBe Some(3)
    m.typeFullName shouldBe "FuzzyBool"
    m.name shouldBe "MAYBE"
    m.code shouldBe "MAYBE"
  }

  "it should correctly parse an enum with values" in {
    cpg.typeDecl.name(".*Color.*").nonEmpty shouldBe true
    // 2 enum values and `label` makes 3 members
    cpg.typeDecl.name(".*Color.*").member.size shouldBe 3
    val List(r, b, l) = cpg.typeDecl.name(".*Color.*").member.l

    l.code shouldBe "java.lang.String label"

    r.code shouldBe "RED(\"Red\")"
    r.astChildren.isCall.size shouldBe 1
    val call = r.astChildren.isCall.head
    call.name shouldBe "Color.<init>"
    call.methodFullName shouldBe "Color.<init>"
    call.order shouldBe 1
    call.astChildren.size shouldBe 1
    call.astChildren.head shouldBe a[Literal]
    call.astChildren.head.code shouldBe "\"Red\""

    b.code shouldBe "BLUE(\"Blue\")"
    b.astChildren.astChildren.size shouldBe 1
    b.astChildren.astChildren.head.code shouldBe "\"Blue\""
  }
}
