package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class ComprehensionCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "list comprehension" should {
    lazy val cpg = code("""x = [i * 2 for i in range(10)]""".stripMargin, "test.py")

    "create a call to range" in {
      cpg.call.codeExact("range(10)").head.name shouldBe "range"
    }

    "create a multiplication operation" in {
      cpg.call.methodFullName(Operators.multiplication).codeExact("i * 2").size shouldBe 1
    }
  }

  "set comprehension" should {
    lazy val cpg = code("""x = {i * 2 for i in range(10)}""".stripMargin, "test.py")

    "create a call to range" in {
      cpg.call.codeExact("range(10)").head.name shouldBe "range"
    }

    "create a multiplication operation" in {
      cpg.call.methodFullName(Operators.multiplication).codeExact("i * 2").size shouldBe 1
    }
  }

  "dict comprehension" should {
    lazy val cpg = code("""x = {k: v for k, v in items.items()}""".stripMargin, "test.py")

    "create a call to items()" in {
      cpg.call.codeExact("items.items()").head.name shouldBe "items"
    }

    "have identifiers for k and v" in {
      cpg.identifier.nameExact("k").size should be > 0
      cpg.identifier.nameExact("v").size should be > 0
    }
  }

  "generator expression" should {
    lazy val cpg = code("""x = sum(i * 2 for i in range(10))""".stripMargin, "test.py")

    "create a call to sum" in {
      cpg.call.codeExact("sum(i * 2 for i in range(10))").head.name shouldBe "sum"
    }

    "create a call to range" in {
      cpg.call.codeExact("range(10)").head.name shouldBe "range"
    }
  }

}
