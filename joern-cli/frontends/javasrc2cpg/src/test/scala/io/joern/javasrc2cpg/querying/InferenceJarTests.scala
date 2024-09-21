package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.JavaSrc2Cpg.DefaultConfig
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.utils.ProjectRoot

class InferenceJarTests extends JavaSrcCode2CpgFixture {

  private val _code: String =
    """
      |class Test {
      |  public void test1() {
      |    System.out.println(Deps.foo());
      |  }
      |}
      |""".stripMargin

  "CPG for code where inference jar for dependencies is provided" should {
    val inferenceJarPath = ProjectRoot.relativise("joern-cli/frontends/javasrc2cpg/src/test/resources/Deps.jar")
    lazy val cpg         = code(_code).withConfig(DefaultConfig.withInferenceJarPaths(Set(inferenceJarPath)))

    "resolve the type for Deps" in {
      val call = cpg.method.name("test1").call.name("foo").head
      call.methodFullName shouldBe "Deps.foo:int()"
      call.typeFullName shouldBe "int"
      call.signature shouldBe "int()"
    }

    "create stubs for elements used in Deps" in {
      cpg.typeDecl.name("Deps").size shouldBe 1
      val depsTypeDecl = cpg.typeDecl.name("Deps").head
      depsTypeDecl.fullName shouldBe "Deps"
      depsTypeDecl.isExternal shouldBe true

      cpg.method.name("foo").size shouldBe 1
      val fooMethod = cpg.method.name("foo").head
      fooMethod.fullName shouldBe "Deps.foo:int()"
      fooMethod.signature shouldBe "int()"
    }
  }

  "CPG for code where inference jar for dependencies is not provided" should {
    lazy val cpg = code(_code)

    "fail to resolve the type for Deps" in {
      val call = cpg.method.name("test1").call.name("foo").head
      call.methodFullName shouldBe s"${Defines.UnresolvedNamespace}.foo:${Defines.UnresolvedSignature}(0)"
      call.signature shouldBe s"${Defines.UnresolvedSignature}(0)"
      call.typeFullName shouldBe "ANY"
    }
  }
}
