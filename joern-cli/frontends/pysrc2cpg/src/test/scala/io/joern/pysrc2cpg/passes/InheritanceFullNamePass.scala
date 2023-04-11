package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language._

class InheritanceFullNamePass extends PySrc2CpgFixture(withOssDataflow = false) {

  "inherited type full names" should {
    lazy val cpg = code(
      """
        |class Foo():
        |  pass
        |""".stripMargin,
      "foo.py"
    ).moreCode(
      """
        |from foo import Foo
        |
        |class Bar(Foo):
        | pass
        |""".stripMargin,
      "bar.py"
    )

    "resolve the type being inherited fully" in {
      def bar = cpg.typeDecl("Bar")
      bar.inheritsFromTypeFullName.l shouldBe Seq("foo.py:<module>.Foo")
      bar.baseType.fullName.l shouldBe Seq("foo.py:<module>.Foo")
    }
  }

}
