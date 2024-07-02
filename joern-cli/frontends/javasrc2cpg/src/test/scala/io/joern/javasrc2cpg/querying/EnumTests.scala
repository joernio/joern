package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*

class EnumTests extends JavaSrcCode2CpgFixture {
  val cpg = code("""
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
      |""".stripMargin)

  "it should parse a basic enum without values" in {
    inside(cpg.typeDecl.name(".*FuzzyBool.*").l) { case List(typeDecl) =>
      typeDecl.code shouldBe "public enum FuzzyBool"

      inside(typeDecl.member.l) { case List(trueMember, falseMember, maybeMember) =>
        trueMember.order shouldBe 1
        trueMember.lineNumber shouldBe Some(3)
        trueMember.columnNumber shouldBe Some(3)
        trueMember.typeFullName shouldBe "FuzzyBool"
        trueMember.name shouldBe "TRUE"
        trueMember.code shouldBe "TRUE"

        falseMember.order shouldBe 2
        falseMember.lineNumber shouldBe Some(4)
        falseMember.columnNumber shouldBe Some(3)
        falseMember.typeFullName shouldBe "FuzzyBool"
        falseMember.name shouldBe "FALSE"
        falseMember.code shouldBe "FALSE"

        maybeMember.order shouldBe 3
        maybeMember.lineNumber shouldBe Some(5)
        maybeMember.columnNumber shouldBe Some(3)
        maybeMember.typeFullName shouldBe "FuzzyBool"
        maybeMember.name shouldBe "MAYBE"
        maybeMember.code shouldBe "MAYBE"
      }
    }
  }

  "it should correctly parse an enum with values" in {
    cpg.typeDecl.name(".*Color.*").nonEmpty shouldBe true
    // 2 enum values and `label` makes 3 members
    cpg.typeDecl.name(".*Color.*").member.size shouldBe 3
    val List(r, b, l) = cpg.typeDecl.name(".*Color.*").member.l

    l.code shouldBe "String label"

    r.code shouldBe "RED(\"Red\")"
    r.astChildren.size shouldBe 0

    b.code shouldBe "BLUE(\"Blue\")"
    b.astChildren.astChildren.size shouldBe 0
  }
}
