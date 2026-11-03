package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.codepropertygraph.generated.ModifierTypes
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
      |
      |  boolean isRed() {
      |    return RED == Color.RED;
      |  }
      |}
      |""".stripMargin)

  "the enum type should extends java.lang.Enum" in {
    cpg.typeDecl.name("FuzzyBool").inheritsFromTypeFullName.l shouldBe List("java.lang.Enum")
  }

  "it should lower enum constants and the constants array to static final members" in {
    val members = cpg.typeDecl.nameExact("FuzzyBool").member.l
    members.map(_.name) shouldBe List("TRUE", "FALSE", "MAYBE", "$VALUES")
    members.take(3).foreach { member =>
      member.modifier.modifierType.l should contain allOf (
        ModifierTypes.PUBLIC,
        ModifierTypes.STATIC,
        ModifierTypes.FINAL
      )
    }
    members.last.typeFullName shouldBe "FuzzyBool[]"
    members.last.modifier.modifierType.l should contain allOf (
      ModifierTypes.PRIVATE,
      ModifierTypes.STATIC,
      ModifierTypes.FINAL
    )
  }

  "it should synthesize the enum API and initialize entries in declaration order" in {
    val enumDecl = cpg.typeDecl.nameExact("Color")
    enumDecl.method.name.l should contain allOf ("values", "valueOf", "<clinit>")
  }

  "it should resolve unqualified enum constants as static field accesses" in {
    val accesses = cpg.typeDecl.nameExact("Color").method.nameExact("isRed").call.nameExact("<operator>.fieldAccess").l
    accesses.map(_.code) should contain("Color.RED")
  }

  "it should parse a basic enum without values" in {
    inside(cpg.typeDecl.name(".*FuzzyBool.*").l) { case List(typeDecl) =>
      typeDecl.code shouldBe "public enum FuzzyBool"

      inside(typeDecl.member.l) { case List(trueMember, falseMember, maybeMember, valuesMember) =>
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

        valuesMember.name shouldBe "$VALUES"
      }
    }
  }

  "it should correctly parse an enum with values" in {
    cpg.typeDecl.name(".*Color.*").nonEmpty shouldBe true
    // 2 enum values, `label`, and the compiler-generated values array make 4 members
    cpg.typeDecl.name(".*Color.*").member.size shouldBe 4
    val List(r, b, l, values) = cpg.typeDecl.name(".*Color.*").member.l

    l.code shouldBe "String label"

    r.code shouldBe "RED(\"Red\")"
    r.modifier.modifierType.l should contain allOf (ModifierTypes.PUBLIC, ModifierTypes.STATIC, ModifierTypes.FINAL)

    b.code shouldBe "BLUE(\"Blue\")"
    b.modifier.modifierType.l should contain allOf (ModifierTypes.PUBLIC, ModifierTypes.STATIC, ModifierTypes.FINAL)

    values.name shouldBe "$VALUES"
  }

  "an enum implementing an interface" should {
    val cpg = code("""
        |package foo;
        |
        |interface Bar {
        |  String describe();
        |}
        |
        |public enum Foo implements Bar {
        |  NORTH,
        |  SOUTH;
        |
        |  public String describe() {
        |    return name();
        |  }
        |}
        |""".stripMargin)

    "inherit from both java.lang.Enum and the interface" in {
      cpg.typeDecl.name("Foo").inheritsFromTypeFullName.l shouldBe List("java.lang.Enum", "foo.Bar")
    }
  }

  "an enum implementing multiple interfaces" should {
    val cpg = code("""
        |package foo;
        |
        |interface Bar {
        |  String describe();
        |}
        |
        |interface Baz {
        |  int priority();
        |}
        |
        |public enum Foo implements Bar, Baz {
        |  NORTH,
        |  SOUTH;
        |
        |  public String describe() {
        |    return name();
        |  }
        |
        |  public int priority() {
        |    return ordinal();
        |  }
        |}
        |""".stripMargin)

    "inherit from java.lang.Enum and both interfaces, in declaration order" in {
      cpg.typeDecl.name("Foo").inheritsFromTypeFullName.l shouldBe List("java.lang.Enum", "foo.Bar", "foo.Baz")
    }
  }

  "an enum with multiple constant-specific class bodies" should {
    val cpg = code("""
        |package foo;
        |
        |public enum Foo {
        |  NORTH {
        |    @Override
        |    String describe() { return "N"; }
        |  },
        |  SOUTH {
        |    @Override
        |    String describe() { return "S"; }
        |  };
        |
        |  abstract String describe();
        |}
        |""".stripMargin)

    "give each constant-specific body its own anonymous type, inheriting from the enum" in {
      cpg.typeDecl.nameExact("Foo$0").inheritsFromTypeFullName.l shouldBe List("foo.Foo")
      cpg.typeDecl.nameExact("Foo$1").inheritsFromTypeFullName.l shouldBe List("foo.Foo")
    }
  }
}
