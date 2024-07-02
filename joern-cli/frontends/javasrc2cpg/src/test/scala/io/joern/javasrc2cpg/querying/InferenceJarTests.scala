package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.JavaSrc2CpgTestContext
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.utils.ProjectRoot
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InferenceJarTests extends AnyFreeSpec with Matchers {

  private val code: String =
    """
      |class Test {
      |  public void test1() {
      |    System.out.println(Deps.foo());
      |  }
      |}
      |""".stripMargin

  "CPG for code where inference jar for dependencies is provided" - {
    val inferenceJarPath = ProjectRoot.relativise("joern-cli/frontends/javasrc2cpg/src/test/resources/Deps.jar")
    lazy val cpg         = JavaSrc2CpgTestContext.buildCpg(code, inferenceJarPaths = Set(inferenceJarPath))

    "it should resolve the type for Deps" in {
      val call = cpg.method.name("test1").call.name("foo").head
      call.methodFullName shouldBe "Deps.foo:int()"
      call.typeFullName shouldBe "int"
      call.signature shouldBe "int()"
    }

    "it should create stubs for elements used in Deps" in {
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

  "CPG for code where inference jar for dependencies is not provided" - {
    lazy val cpg = JavaSrc2CpgTestContext.buildCpg(code)

    "it should fail to resolve the type for Deps" in {
      val call = cpg.method.name("test1").call.name("foo").head
      call.methodFullName shouldBe s"${Defines.UnresolvedNamespace}.foo:${Defines.UnresolvedSignature}(0)"
      call.signature shouldBe s"${Defines.UnresolvedSignature}(0)"
      call.typeFullName shouldBe "ANY"
    }
  }
}
