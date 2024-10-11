package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ClassCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {
  "class" should {
    val cpg = code("""class Foo:
        |  pass
        |""".stripMargin)
    "have correct instance class type and typeDecl" in {
      cpg.typ.name("Foo").fullName.head shouldBe "Test0.py:<module>.Foo"
      val typeDecl = cpg.typeDecl.name("Foo").head
      typeDecl.fullName shouldBe "Test0.py:<module>.Foo"
      typeDecl.astParent shouldBe cpg.method.name("<module>").head
    }

    "have correct meta class type and typeDecl" in {
      cpg.typ.name("Foo<meta>").fullName.head shouldBe "Test0.py:<module>.Foo<meta>"
      val typeDecl = cpg.typeDecl.name("Foo<meta>").head
      typeDecl.fullName shouldBe "Test0.py:<module>.Foo<meta>"
      typeDecl.astParent shouldBe cpg.method.name("<module>").head
    }

    "have correct meta class call handler type and typeDecl" in {
      cpg.typ.name("<metaClassCallHandler>").fullName.head shouldBe
        "Test0.py:<module>.Foo.<metaClassCallHandler>"
      val typeDecl = cpg.typeDecl.name("<metaClassCallHandler>").head
      typeDecl.fullName shouldBe "Test0.py:<module>.Foo.<metaClassCallHandler>"
      typeDecl.astParent shouldBe cpg.typeDecl.name("Foo<meta>").head
    }

    "have correct fake new type and typeDecl" in {
      cpg.typ.name("<fakeNew>").fullName.head shouldBe
        "Test0.py:<module>.Foo.<fakeNew>"
      val typeDecl = cpg.typeDecl.name("<fakeNew>").head
      typeDecl.fullName shouldBe "Test0.py:<module>.Foo.<fakeNew>"
      typeDecl.astParent shouldBe cpg.typeDecl.name("Foo<meta>").head
    }
  }

  "class meta call handler" should {
    "have no self parameter if self is explicit" in {
      val cpg = code("""class Foo:
          |  def __init__(self, x):
          |    pass
          |""".stripMargin)

      val handlerMethod = cpg.method.name("<metaClassCallHandler>").head
      handlerMethod.fullName shouldBe "Test0.py:<module>.Foo.<metaClassCallHandler>"
      handlerMethod.lineNumber shouldBe Some(2)

      handlerMethod.parameter.size shouldBe 1
      val xParameter = handlerMethod.parameter.head
      xParameter.name shouldBe "x"

    }

    "have no self parameter if self is in varargs" in {
      val cpg = code("""class Foo:
          |  def __init__(*x):
          |    pass
          |""".stripMargin)

      val handlerMethod = cpg.method.name("<metaClassCallHandler>").head
      handlerMethod.fullName shouldBe "Test0.py:<module>.Foo.<metaClassCallHandler>"
      handlerMethod.lineNumber shouldBe Some(2)

      handlerMethod.parameter.size shouldBe 1
      val xParameter = handlerMethod.parameter.head
      xParameter.name shouldBe "x"

    }

    "have correct full name for func1 method in class" in {
      val cpg = code("""class Foo:
                       |  def func1(self):
                       |    pass
                       |""".stripMargin)

      val func1 = cpg.method.name("func1").head
      func1.fullName shouldBe "Test0.py:<module>.Foo.func1"
    }

    "have correct full name for <body> method in class" in {
      val cpg = code("""class Foo:
                       |  pass
                       |""".stripMargin)

      val func1 = cpg.method.name("<body>").head
      func1.fullName shouldBe "Test0.py:<module>.Foo.<body>"
    }

    "have correct parameter index for method in class" in {
      val cpg = code("""class Foo:
                       |  def method(self):
                       |    pass
                       |""".stripMargin)
      cpg.method.name("method").parameter.name("self").index.head shouldBe 0
    }

    "have correct parameter index for static method in class" in {
      val cpg = code("""class Foo:
                       |  @staticmethod
                       |  def method(x):
                       |    pass
                       |""".stripMargin)
      cpg.method.name("method").parameter.name("x").index.head shouldBe 1
    }
  }

}
