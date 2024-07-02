package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*
import io.joern.javasrc2cpg.Config

class LombokTests extends JavaSrcCode2CpgFixture {
  private val config = Config().withDelombokMode("default")

  "source in a mixed directory structure" should {
    val cpg = code(
      """
        |package foo;
        |import lombok.Getter;
        |
        |public class Foo {
        |  @Getter private int value = 42;
        |}
        |""".stripMargin,
      fileName = "src/main/java/foo/Foo.java"
    ).moreCode(
      """
        |package bar;
        |import lombok.Getter;
        |
        |public class Bar {
        |  @Getter private int value = 42;
        |}
        |""".stripMargin,
      fileName = "src/main/java/bar/Bar.java"
    ).moreCode(
      """
        |package baz;
        |import lombok.Getter;
        |
        |public class Baz {
        |  @Getter private int value = 42;
        |}
        |""".stripMargin,
      fileName = "Baz.java"
    ).withConfig(config)

    "generate all the getValue methods" in {
      cpg.method.name("getValue").fullName.toSet shouldBe Set(
        "foo.Foo.getValue:int()",
        "bar.Bar.getValue:int()",
        "baz.Baz.getValue:int()"
      )
    }

    "not give the delomboked filenames" in {
      List("Foo", "Bar", "Baz").foreach { declName =>
        cpg.typeDecl.name(declName).filename.head.contains("lombok") shouldBe false
      }
    }
  }

  "source in the standard java directory structure" should {
    val cpg = code(
      """
        |package foo;
        |import lombok.Getter;
        |
        |public class Foo {
        |  @Getter private int value = 42;
        |}
        |""".stripMargin,
      fileName = "src/main/java/foo/Foo.java"
    ).withConfig(config)

    "delombok the source correctly" in {
      cpg.method.name("getValue").l match {
        case method :: Nil =>
          method.fullName shouldBe "foo.Foo.getValue:int()"
          method.body.astChildren.size shouldBe 1
          method.filename.contains("delombok") shouldBe false

        case result => fail(s"Expected single getValue method but got $result")
      }
    }

    "not give the delomboked filename" in {
      cpg.typeDecl.name("Foo").filename.head.contains("lombok") shouldBe false
    }
  }

  "basic source with lombok annotations" should {
    val cpg = code(
      """
        |import lombok.Getter;
        |
        |public class Foo {
        |    @Getter private int value = 42;
        |}""".stripMargin,
      fileName = "Foo.java"
    ).withConfig(config)

    "delombok the source correctly" in {
      cpg.method.name("getValue").l match {
        case method :: Nil =>
          method.fullName shouldBe "Foo.getValue:int()"
          method.body.astChildren.size shouldBe 1
          method.filename.contains("delombok") shouldBe false

        case result => fail(s"Expected single getValue method but got $result")
      }
    }

    "not give the delomboked filename" in {
      cpg.typeDecl.name("Foo").filename.head.contains("lombok") shouldBe false
    }
  }

  "source with lombok annotations should have correct type information" in {
    val cpg = code(
      """
        |import lombok.extern.java.Log;
        |import lombok.Getter;
        |
        |@Log
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
    ).withConfig(config)

    // Getter type should be resolved since the getter code is included in the delombok source.
    cpg.call.name("getFirstName").head.methodFullName shouldBe "Foo.getFirstName:java.lang.String()"
    // Member should be created since we're processing full delombok source.
    cpg.member.name("log").head.typeFullName shouldBe "java.util.logging.Logger"
  }
}

class LombokTypesOnlyTests extends JavaSrcCode2CpgFixture {
  "source with some lombok annotations should have correct type information" in {
    val cpg = code(
      """
       |import lombok.Getter;
       |import lombok.extern.java.Log;
       |
       |@Log
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
    ).withConfig(Config().withDelombokMode("types-only"))

    // Getter type should be resolved since the getter code is included in the delombok source used for type info.
    cpg.call.name("getFirstName").head.methodFullName shouldBe "Foo.getFirstName:java.lang.String()"
    // Log member should not be found since it hasn't been generated in the source we scan.
    cpg.member.name("log").isEmpty shouldBe true
  }
}

class NoLombokTests extends JavaSrcCode2CpgFixture {
  "source with some lombok annotations should have correct type information" in {
    val cpg = code(
      """
       |import lombok.Getter;
       |import lombok.extern.java.Log;
       |
       |@Log
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

    // Getter type should be unresolved since it's not available in source processed or in type info source.
    val unresolvedName = s"Foo.getFirstName:${Defines.UnresolvedSignature}(0)"
    cpg.call.name("getFirstName").head.methodFullName shouldBe unresolvedName
    // Log member should not be found since it hasn't been generated in the source we scan.
    cpg.member.name("log").isEmpty shouldBe true
  }
}
