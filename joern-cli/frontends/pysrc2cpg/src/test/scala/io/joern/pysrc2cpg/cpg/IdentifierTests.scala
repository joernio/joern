package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

import java.io.File

class IdentifierTests extends PySrc2CpgFixture(withOssDataflow = false) {
  "an identifier assigned to a class object" should {
    val cpg = code(
      """
        |class Foo:
        | pass
        |""".stripMargin,
      "foo.py"
    ).moreCode(
      """
        |from foo import Foo
        |""".stripMargin,
      Seq("bar", "__init__.py").mkString(File.separator)
    ).moreCode(
      """
        |import bar
        |
        |baz = bar.Foo()
        |""".stripMargin,
      "baz.py"
    )

    "have correct typeFullName" in {
      inside(cpg.identifier("baz").l) { case baz :: Nil =>
        baz.typeFullName shouldBe "foo.py:<module>.Foo"
      }
    }
  }
}
