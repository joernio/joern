package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._

class EnumTests extends JimpleCodeToCpgFixture {
  override val code: String =
    """
      |enum FuzzyBool {
      |  TRUE,
      |  FALSE,
      |  MAYBE
      |}
      |
      |enum Color {
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

  "it should contain the basic enum methods" in {
    cpg.typeDecl.name(".*FuzzyBool.*").method.filterNot(_.name.contains("$")).size shouldBe 4
    val List(values, valueOf, constructor, staticInit) =
      cpg.typeDecl.name(".*FuzzyBool.*").method.filterNot(_.name.contains("$")).l

    values.order shouldBe 1
    values.name shouldBe "values"
    values.lineNumber shouldBe Some(1)

    valueOf.order shouldBe 2
    valueOf.name shouldBe "valueOf"
    valueOf.lineNumber shouldBe Some(1)

    constructor.order shouldBe 3
    constructor.name shouldBe "<init>"
    constructor.lineNumber shouldBe Some(1)

    staticInit.order shouldBe 4
    staticInit.name shouldBe "<clinit>"
    staticInit.lineNumber shouldBe Some(2)
  }

  "it should parse a basic enum without values" in {
    cpg.typeDecl.name(".*FuzzyBool.*").nonEmpty shouldBe true
    cpg.typeDecl.name(".*FuzzyBool.*").member.size shouldBe 4
    val List(t, f, m, v) = cpg.typeDecl.name(".*FuzzyBool.*").member.l

    t.lineNumber shouldBe None
    t.columnNumber shouldBe None
    t.typeFullName shouldBe "FuzzyBool"
    t.name shouldBe "TRUE"
    t.code shouldBe "TRUE"

    f.lineNumber shouldBe None
    f.columnNumber shouldBe None
    f.typeFullName shouldBe "FuzzyBool"
    f.name shouldBe "FALSE"
    f.code shouldBe "FALSE"

    m.lineNumber shouldBe None
    m.columnNumber shouldBe None
    m.typeFullName shouldBe "FuzzyBool"
    m.name shouldBe "MAYBE"
    m.code shouldBe "MAYBE"

    v.lineNumber shouldBe None
    v.columnNumber shouldBe None
    v.typeFullName shouldBe "FuzzyBool[]"
    v.name shouldBe "$VALUES"
    v.code shouldBe "FuzzyBool[] $VALUES"
  }

  "it should correctly parse an enum with values" in {
    cpg.typeDecl.name(".*Color.*").nonEmpty shouldBe true
    // 2 enum values, `label`, and $VALUES makes 4 members
    cpg.typeDecl.name(".*Color.*").member.size shouldBe 4
    val List(r, b, l, _) = cpg.typeDecl.name(".*Color.*").member.l

    l.code shouldBe "java.lang.String label"

    val List(redCall, blueCall) = cpg.typeDecl
      .name(".*Color.*")
      .method
      .name("<clinit>")
      .ast
      .isCall
      .code(".*.Color(.+, \\d+, .+)")
      .l

    r.code shouldBe "RED"

    redCall.name shouldBe "<init>"
    redCall.methodFullName shouldBe "Color.<init>:void(java.lang.String,int,java.lang.String)"
    redCall.astChildren.size shouldBe 4
    redCall.astChildren.last shouldBe a[Literal]
    redCall.astChildren.last.code shouldBe "\"Red\""

    b.code shouldBe "BLUE"

    blueCall.name shouldBe "<init>"
    blueCall.methodFullName shouldBe "Color.<init>:void(java.lang.String,int,java.lang.String)"
    blueCall.astChildren.size shouldBe 4
    blueCall.astChildren.last shouldBe a[Literal]
    blueCall.astChildren.last.code shouldBe "\"Blue\""
  }
}
