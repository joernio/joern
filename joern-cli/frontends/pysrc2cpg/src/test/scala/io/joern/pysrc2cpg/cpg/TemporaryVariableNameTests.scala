package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class TemporaryVariableNameTests extends PySrc2CpgFixture {
  "lowering temporary variables" should {
    "avoid source names that occur later in the same scope" in {
      val cpg = code("tmp0, value = source")

      cpg.call.code("tmp1 = source").size shouldBe 1
      cpg.method.name("<module>").local.name("tmp0").size shouldBe 1
      cpg.method.name("<module>").local.name("tmp1").size shouldBe 1
    }

    "avoid parameter names" in {
      val cpg = code("""def f(tmp0):
          |    left, right = source
          |""".stripMargin)

      cpg.call.code("tmp1 = source").size shouldBe 1
      cpg.method.name("f").local.name("tmp1").size shouldBe 1
    }

    "avoid names captured from enclosing scopes" in {
      val cpg = code("""def foo():
          |    tmp0 = 1
          |    def bar():
          |        left, right = source
          |        print(tmp0)
          |    bar()
          |foo()
          |""".stripMargin)

      cpg.method.name("bar").ast.isCall.code("tmp1 = source").size shouldBe 1
      cpg.method.name("bar").local.name("tmp1").size shouldBe 1
    }

    "avoid import aliases for prefixed temporaries" in {
      val cpg = code("""import manager as manager_tmp0
          |with context():
          |    pass
          |""".stripMargin)

      cpg.identifier.name("manager_tmp1").size should be > 0
      cpg.identifier.name("enter_tmp0").size should be > 0
    }

    "avoid names in comprehension contexts" in {
      val cpg = code("""result = [tmp0 for tmp0 in values]
          |""".stripMargin)

      cpg.call.code("tmp1 = \\[\\]").size shouldBe 1
    }

    "avoid names in exception-handler contexts" in {
      val cpg = code("""try:
          |    pass
          |except Exception as tmp0:
          |    left, right = source
          |""".stripMargin)

      cpg.call.code("tmp1 = source").size shouldBe 1
    }

    "start counters independently in sibling functions" in {
      val cpg = code("""def first():
          |    a, b = one
          |def second():
          |    c, d = two
          |""".stripMargin)

      cpg.method.name("first").ast.isCall.code("tmp0 = one").size shouldBe 1
      cpg.method.name("second").ast.isCall.code("tmp0 = two").size shouldBe 1
    }

    "keep temporary names unique across sibling comprehensions" in {
      val cpg = code("""first = [x for x in one]
          |second = [y for y in two]
          |""".stripMargin)

      cpg.call.code("tmp0 = \\[\\]").size shouldBe 1
      cpg.call.code("tmp2 = \\[\\]").size shouldBe 1
      cpg.method.name("<module>").local.name("tmp0").size shouldBe 1
      cpg.method.name("<module>").local.name("tmp2").size shouldBe 1
    }
  }
}
