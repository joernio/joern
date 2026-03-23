package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.utils.{HttpArtifact, NetworkTest}
import io.shiftleft.semanticcpg.language.*

class ModuleTests extends JavaSrcCode2CpgFixture {

  "module imports" when {
    "a type is imported from a JDK module" should {
      val cpg = code("""
          |import module java.base;
          |
          |public class Test {
          |  void test() {
          |    ArrayList<String> list = new ArrayList<>();
          |  }
          |}
          |""".stripMargin)

      "represent the module import correctly" in {
        inside(cpg.imports.l) { case List(javaBaseImport) =>
          javaBaseImport.importedEntity shouldBe Some("java.base")
          javaBaseImport.isModuleImport shouldBe Some(true)
        }
      }

      "have a resolved type in the CPG" in {
        inside(cpg.local.name("list").l) { case List(listLocal) =>
          listLocal.typeFullName shouldBe "java.util.ArrayList"
        }
      }
    }
  }

  "a type is imported from source" should {
    val cpg = code(
      """
        |module com.testmodule {
        |    exports com.testpackage;
        |}
        |""".stripMargin,
      fileName = "module-info.java"
    ).moreCode("""
        |package com.testpackage;
        |
        |class Foo {}
        |""".stripMargin)
      .moreCode("""
        |import module com.testmodule;
        |
        |public class Test {
        |  void test() {
        |  Foo f = new Foo();
        |  }
        |}
        |""".stripMargin)

    "have a resolved type in the CPG" in {
      inside(cpg.local.name("f").l) { case List(fLocal) =>
        fLocal.typeFullName shouldBe "com.testpackage.Foo"
      }
    }
  }

  "a type is imported from an inference jar" should {
    "have a resolved type in the CPG" taggedAs NetworkTest in {
      val cpg = code("""
          |import module com.cedarsoftware.util;
          |
          |class Foo {
          |  public void foo() {
          |    ThreadedLRUCacheStrategy strategy = new ThreadedLRUCacheStrategy(2);
          |  }
          |}
          |""".stripMargin)
        .withRemoteInferenceJars(
          HttpArtifact(
            "https://repo1.maven.org/maven2/com/cedarsoftware/java-util/4.96.0/java-util-4.96.0.jar",
            "1181ed04882b4bee69228ed5093e9b077d0c054a8f39ba22ea8a749d69e22f72"
          )
        )

      inside(cpg.local.name("strategy").l) { case List(strategyLocal) =>
        strategyLocal.typeFullName shouldBe "com.cedarsoftware.util.cache.ThreadedLRUCacheStrategy"
      }
    }
  }
}
