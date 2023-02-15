package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language._

class ClassCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {
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
  }

}
