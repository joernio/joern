package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class FunctionRedefinitionCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {
  private def methodFullNames(cpg: Cpg, name: String): Set[String] =
    cpg.method.name(name).fullName.toSet

  "two top-level functions with the same name" should {
    val cpg = code("""def f():
        |  return "zeroth"
        |def f():
        |  return "first"
        |""".stripMargin)

    "produce two distinct method full names" in {
      methodFullNames(cpg, "f") shouldBe Set("Test0.py:<module>.f<redefined>0", "Test0.py:<module>.f")
    }

    "route each body to its own method" in {
      inside(cpg.method.fullNameExact("Test0.py:<module>.f<redefined>0").ast.isLiteral.code.l) { case List(code) =>
        code shouldBe "\"zeroth\""
      }
      inside(cpg.method.fullNameExact("Test0.py:<module>.f").ast.isLiteral.code.l) { case List(code) =>
        code shouldBe "\"first\""
      }
    }
  }

  "three top-level functions with the same name" should {
    val cpg = code("""def f(): pass
        |def f(): pass
        |def f(): pass
        |""".stripMargin)

    "produce three distinct method full names" in {
      methodFullNames(cpg, "f") shouldBe Set(
        "Test0.py:<module>.f<redefined>0",
        "Test0.py:<module>.f<redefined>1",
        "Test0.py:<module>.f"
      )
    }
  }

  "same-name function in different branches of if/else" should {
    val cpg = code("""if cond:
        |  def f(): pass
        |else:
        |  def f(): pass
        |""".stripMargin)

    "produce two distinct method full names since branches share module scope" in {
      methodFullNames(cpg, "f") shouldBe Set("Test0.py:<module>.f<redefined>0", "Test0.py:<module>.f")
    }
  }

  "function inside another function does not collide with same-name function at module level" should {
    val cpg = code("""def f(): pass
        |def outer():
        |  def f(): pass
        |""".stripMargin)

    "produce plain full names for both" in {
      methodFullNames(cpg, "f") shouldBe Set("Test0.py:<module>.f", "Test0.py:<module>.outer.f")
    }
  }

  "two same-name functions inside the same enclosing function scope" should {
    val cpg = code("""def outer():
        |  def f(): pass
        |  def f(): pass
        |""".stripMargin)

    "mangle the earlier definition and keep the last one unmangled" in {
      methodFullNames(cpg, "f") shouldBe Set(
        "Test0.py:<module>.outer.f<redefined>0",
        "Test0.py:<module>.outer.f"
      )
    }
  }

  "two same-name methods inside the same class scope" should {
    val cpg = code("""class Foo:
        |  def m(self): pass
        |  def m(self): pass
        |""".stripMargin)

    "mangle the earlier definition and keep the last one unmangled" in {
      methodFullNames(cpg, "m") shouldBe Set(
        "Test0.py:<module>.Foo.m<redefined>0",
        "Test0.py:<module>.Foo.m"
      )
    }

    "bind the last definition on the instance TypeDecl" in {
      cpg.typeDecl
        .fullNameExact("Test0.py:<module>.Foo")
        .member
        .name("m")
        .dynamicTypeHintFullName
        .l shouldBe List("Test0.py:<module>.Foo.m")
    }
  }

  "same-name functions across try/except/finally bodies" should {
    val cpg = code("""try:
        |  def f(): pass
        |except Exception:
        |  def f(): pass
        |finally:
        |  def f(): pass
        |""".stripMargin)

    "produce three distinct method full names" in {
      methodFullNames(cpg, "f") shouldBe Set(
        "Test0.py:<module>.f<redefined>0",
        "Test0.py:<module>.f<redefined>1",
        "Test0.py:<module>.f"
      )
    }
  }

  "same-name functions inside different match cases" should {
    val cpg = code("""match x:
        |  case 1:
        |    def f(): pass
        |  case _:
        |    def f(): pass
        |""".stripMargin)

    "produce two distinct method full names" in {
      methodFullNames(cpg, "f") shouldBe Set("Test0.py:<module>.f<redefined>0", "Test0.py:<module>.f")
    }
  }

  "two same-name async functions inside the same enclosing function scope" should {
    val cpg = code("""def outer():
        |  async def f(): pass
        |  async def f(): pass
        |""".stripMargin)

    "mangle the earlier definition and keep the last one unmangled" in {
      methodFullNames(cpg, "f") shouldBe Set(
        "Test0.py:<module>.outer.f<redefined>0",
        "Test0.py:<module>.outer.f"
      )
    }
  }

  "same-name function inside a while body and at module level" should {
    val cpg = code("""while cond:
        |  def f(): pass
        |def f(): pass
        |""".stripMargin)

    "produce two distinct method full names since the while body shares module scope" in {
      methodFullNames(cpg, "f") shouldBe Set("Test0.py:<module>.f<redefined>0", "Test0.py:<module>.f")
    }
  }

  "calls interleaved between two same-name top-level function definitions" should {
    val cpg = code("""def f():
        |    print("first f")
        |
        |f()
        |
        |def f():
        |    print("second f")
        |
        |f()
        |""".stripMargin)

    "produce two method nodes with distinct full names, each with its own body" in {
      methodFullNames(cpg, "f") shouldBe Set("Test0.py:<module>.f<redefined>0", "Test0.py:<module>.f")

      inside(cpg.method.fullNameExact("Test0.py:<module>.f<redefined>0").ast.isLiteral.code.l) { case List(code) =>
        code shouldBe "\"first f\""
      }
      inside(cpg.method.fullNameExact("Test0.py:<module>.f").ast.isLiteral.code.l) { case List(code) =>
        code shouldBe "\"second f\""
      }
    }

    "produce two call sites to f" in {
      cpg.call.name("f").size shouldBe 2
    }
  }

  "same-name nested function inside two same-name outer functions" should {
    val cpg = code("""def outer():
        |  def inner(): return "first"
        |def outer():
        |  def inner(): return "second"
        |""".stripMargin)

    "produce distinct full names for each inner because outer's mangled suffix propagates" in {
      methodFullNames(cpg, "inner") shouldBe Set(
        "Test0.py:<module>.outer<redefined>0.inner",
        "Test0.py:<module>.outer.inner"
      )
    }

    "route each inner body to its own method" in {
      inside(cpg.method.fullNameExact("Test0.py:<module>.outer<redefined>0.inner").ast.isLiteral.code.l) {
        case List(code) => code shouldBe "\"first\""
      }
      inside(cpg.method.fullNameExact("Test0.py:<module>.outer.inner").ast.isLiteral.code.l) { case List(code) =>
        code shouldBe "\"second\""
      }
    }
  }

  "same-name function and class in the same scope" should {
    val cpg = code("""def A(): pass
        |class A: pass
        |""".stripMargin)

    "not mangle either since they occupy separate def buckets" in {
      methodFullNames(cpg, "A") shouldBe Set("Test0.py:<module>.A")
      cpg.typeDecl.name("A").fullName.toSet shouldBe Set("Test0.py:<module>.A")
    }
  }
}
