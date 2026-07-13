package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class ClassRedefinitionCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {
  private def typeDeclFullNames(cpg: Cpg, name: String): Set[String] =
    cpg.typeDecl.name(name).fullName.toSet

  "two top-level classes with the same name" should {
    val cpg = code("""class A:
        |  def m(self):
        |      print("zeroth")
        |class A:
        |  def m(self):
        |      print("first")
        |""".stripMargin)

    "produce two distinct instance TypeDecl full names" in {
      val fullNames = cpg.typeDecl.name("A").fullName.toSet
      fullNames shouldBe Set("Test0.py:<module>.A<duplicate>0", "Test0.py:<module>.A")

      inside(
        cpg.typeDecl
          .fullNameExact("Test0.py:<module>.A<duplicate>0")
          .ast
          .isCall(".*print.*")
          .argument
          .argumentIndex(1)
          .l
      ) { case List(firstAArg) =>
        firstAArg.code shouldBe "\"zeroth\""
      }

      inside(cpg.typeDecl.fullNameExact("Test0.py:<module>.A").ast.isCall(".*print.*").argument.argumentIndex(1).l) {
        case List(secondAArg) =>
          secondAArg.code shouldBe "\"first\""
      }
    }

    "produce two distinct meta TypeDecl full names" in {
      val fullNames = cpg.typeDecl.name("A<meta>").fullName.toSet
      fullNames shouldBe Set("Test0.py:<module>.A<meta><duplicate>0", "Test0.py:<module>.A<meta>")
    }
  }

  "three top-level classes with the same name" should {
    val cpg = code("""class A: print("zeroth")
        |class A: print("first")
        |class A: print("second")
        |""".stripMargin)

    "produce three distinct instance TypeDecl full names" in {
      val fullNames = cpg.typeDecl.name("A").fullName.toSet
      fullNames shouldBe Set(
        "Test0.py:<module>.A<duplicate>0",
        "Test0.py:<module>.A<duplicate>1",
        "Test0.py:<module>.A"
      )
    }
  }

  "same-name class in different branches of if/else" should {
    val cpg = code("""if cond:
        |  class A: pass
        |else:
        |  class A: pass
        |""".stripMargin)

    "produce two distinct instance TypeDecl full names" in {
      val fullNames = cpg.typeDecl.name("A").fullName.toSet
      fullNames shouldBe Set("Test0.py:<module>.A<duplicate>0", "Test0.py:<module>.A")
    }
  }

  "class inside a function does not collide with same-name class at module level" should {
    val cpg = code("""class A: pass
        |def f():
        |  class A: pass
        |""".stripMargin)

    "produce plain full names for both" in {
      val fullNames = cpg.typeDecl.name("A").fullName.toSet
      fullNames shouldBe Set("Test0.py:<module>.A", "Test0.py:<module>.f.A")
    }
  }

  "two same-name classes inside the same function scope" should {
    val cpg = code("""def f():
        |  class A: pass
        |  class A: pass
        |""".stripMargin)

    "mangle the earlier definition and keep the last one unmangled" in {
      val fullNames = cpg.typeDecl.name("A").fullName.toSet
      fullNames shouldBe Set("Test0.py:<module>.f.A<duplicate>0", "Test0.py:<module>.f.A")
    }
  }

  "two same-name nested classes inside the same enclosing class" should {
    val cpg = code("""class Outer:
        |  class Inner: pass
        |  class Inner: pass
        |""".stripMargin)

    "mangle the earlier Inner and keep the last Inner unmangled" in {
      typeDeclFullNames(cpg, "Inner") shouldBe Set(
        "Test0.py:<module>.Outer.Inner<duplicate>0",
        "Test0.py:<module>.Outer.Inner"
      )
      typeDeclFullNames(cpg, "Outer") shouldBe Set("Test0.py:<module>.Outer")
    }
  }

  "same-name classes across try/except/finally bodies" should {
    val cpg = code("""try:
        |  class A: pass
        |except Exception:
        |  class A: pass
        |finally:
        |  class A: pass
        |""".stripMargin)

    "produce three distinct instance TypeDecl full names" in {
      typeDeclFullNames(cpg, "A") shouldBe Set(
        "Test0.py:<module>.A<duplicate>0",
        "Test0.py:<module>.A<duplicate>1",
        "Test0.py:<module>.A"
      )
    }
  }

  "same-name classes inside different match cases" should {
    val cpg = code("""match x:
        |  case 1:
        |    class A: pass
        |  case _:
        |    class A: pass
        |""".stripMargin)

    "produce two distinct instance TypeDecl full names" in {
      typeDeclFullNames(cpg, "A") shouldBe Set("Test0.py:<module>.A<duplicate>0", "Test0.py:<module>.A")
    }
  }

  "two same-name classes inside the same async function scope" should {
    val cpg = code("""async def f():
        |  class A: pass
        |  class A: pass
        |""".stripMargin)

    "mangle the earlier definition and keep the last one unmangled" in {
      typeDeclFullNames(cpg, "A") shouldBe Set("Test0.py:<module>.f.A<duplicate>0", "Test0.py:<module>.f.A")
    }
  }

  "same-name class inside a while body and at module level" should {
    val cpg = code("""while cond:
        |  class A: pass
        |class A: pass
        |""".stripMargin)

    "produce two distinct instance TypeDecl full names since the while body shares module scope" in {
      typeDeclFullNames(cpg, "A") shouldBe Set("Test0.py:<module>.A<duplicate>0", "Test0.py:<module>.A")
    }
  }
}
