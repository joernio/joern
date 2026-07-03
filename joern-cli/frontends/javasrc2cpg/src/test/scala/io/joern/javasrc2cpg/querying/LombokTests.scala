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
      inside(cpg.method.name("getValue").l) { case method :: Nil =>
        method.fullName shouldBe "foo.Foo.getValue:int()"
        method.body.astChildren.size shouldBe 1
        method.filename.contains("delombok") shouldBe false
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
      inside(cpg.method.name("getValue").l) { case method :: Nil =>
        method.fullName shouldBe "Foo.getValue:int()"
        method.body.astChildren.size shouldBe 1
        method.filename.contains("delombok") shouldBe false
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

class SimpleDelombokProjectTests extends JavaSrcCode2CpgFixture {

  "delombok on a maven project with multiple source roots" should {
    val fileA =
      """package com.example.a;
        |
        |import lombok.Data;
        |
        |@Data
        |public class FileA {
        |    private String name;
        |}
        |""".stripMargin

    val fileB =
      """package com.example.b;
        |
        |import com.example.a.FileA;
        |import lombok.ToString;
        |
        |@ToString
        |public class FileB {
        |
        |    private FileA fileA;
        |
        |    @ToString.Include(name = "aName")
        |    String describeA() {
        |        return fileA.getName();
        |    }
        |}
        |""".stripMargin

    val fileB2 =
      """package com.example.b;
        |
        |import com.example.a.FileA;
        |import lombok.ToString;
        |
        |@ToString
        |public class FileB2 {
        |
        |    private FileA fileA;
        |
        |    @ToString.Include(name = "aName")
        |    String describeA() {
        |        return fileA.getName();
        |    }
        |}
        |""".stripMargin

    val fileC =
      """package com.example.c;
        |
        |import com.example.a.FileA;
        |import com.example.b.FileB;
        |import lombok.ToString;
        |
        |@ToString
        |public class FileC {
        |
        |    private FileA fileA;
        |    private FileB fileB;
        |
        |    @ToString.Include(name = "combined")
        |    String describeCombined() {
        |        return fileA.getName() + " / " + fileB.toString();
        |    }
        |}
        |""".stripMargin

    val cpg = code(fileA, "src/main/java/com/example/a/FileA.java")
      .moreCode(fileB, "src/main/java-b/com/example/b/FileB.java")
      .moreCode(fileB2, "src/main/java-b/com/example/b/FileB2.java")
      .moreCode(fileC, "src/main/java-c/com/example/c/FileC.java")
      .withConfig(Config().withDelombokMode("default"))

    "generate the @Data getters and setters on FileA" in {
      cpg.method.fullName.toSet should contain allOf (
        "com.example.a.FileA.getName:java.lang.String()",
        "com.example.a.FileA.setName:void(java.lang.String)"
      )
    }

    "generate the @ToString-produced toString on FileB, FileB2, and FileC" in {
      val toStringOwners = cpg.method.name("toString").typeDecl.fullName.toSet
      toStringOwners should contain allOf (
        "com.example.b.FileB",
        "com.example.b.FileB2",
        "com.example.c.FileC"
      )
    }

    "resolve the cross-source-root FileA#getName call from FileB#describeA" in {
      val call = cpg.typeDecl.fullNameExact("com.example.b.FileB").method.name("describeA").call.name("getName").head
      call.methodFullName shouldBe "com.example.a.FileA.getName:java.lang.String()"
    }

    "resolve the cross-source-root FileA and FileB references from FileC#describeCombined" in {
      val method      = cpg.typeDecl.fullNameExact("com.example.c.FileC").method.name("describeCombined").head
      val getNameCall = method.call.name("getName").head
      getNameCall.methodFullName shouldBe "com.example.a.FileA.getName:java.lang.String()"

      val toStringCall = method.call.name("toString").head
      toStringCall.methodFullName shouldBe "com.example.b.FileB.toString:java.lang.String()"
    }

    "not expose delomboked temp paths on typeDecl filenames" in {
      cpg.typeDecl.fullName("com.example\\..*").filename.foreach { filename =>
        filename.contains("delombok") shouldBe false
      }
    }
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
