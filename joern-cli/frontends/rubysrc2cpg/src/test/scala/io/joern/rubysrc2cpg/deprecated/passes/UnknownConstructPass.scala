package io.joern.rubysrc2cpg.deprecated.passes

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.utils.Environment
import io.joern.x2cpg.utils.Environment.OperatingSystemType
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

class UnknownConstructPass extends RubyCode2CpgFixture(useDeprecatedFrontend = true) {

  "invalid assignment" ignore {
    val cpg = code("""
        |a = 1
        |b = [[]
        |c = 2
        |""".stripMargin)

    "be ignored" in {
      val List(a, c) = cpg.assignment.l
      a.target.code shouldBe "a"
      a.source.code shouldBe "1"

      c.target.code shouldBe "c"
      c.source.code shouldBe "2"
    }
  }

  "invalid method body" ignore {
    val cpg = code("""
        |x = 1
        |def random(a)
        | b = 3 + 2)
        | y = 2
        |end
        |z = 2
        |""".stripMargin)

    "preserve code around it and show the rest of the method body" in {
      val List(x, y, z, random) = cpg.assignment.l
      x.target.code shouldBe "x"
      x.source.code shouldBe "1"

      y.target.code shouldBe "y"
      y.source.code shouldBe "2"

      z.target.code shouldBe "z"
      z.source.code shouldBe "2"

      random.target.code shouldBe "random"
      random.source.code shouldBe "def random(...)"

      val List(m: Method) = cpg.method.nameExact("random").l
      val List(_y)        = m.assignment.l
      y.id() shouldBe _y.id()
    }
  }

  "unrecognized token in the RHS of an assignment" ignore {
    val cpg = code("""
        |x = \!
        |y = 1
        |""".stripMargin)

    "be ignored" in {
      val List(y) = cpg.assignment.l

      y.target.code shouldBe "y"
      y.source.code shouldBe "1"
    }
  }

  "an attempted fix" ignore {
    val cpg = code("""
        |class DerivedClass < BaseClass
        | KEYS = %w(
        |  id1
        |  id2
        |  id3
        | ).freeze
        |end
        |""".stripMargin)

    "not cause an infinite loop once the last line is blanked out, at the cost of the structure (in Unix)" in {
      cpg.typeDecl("DerivedClass").size shouldBe
        (if (Environment.operatingSystem == OperatingSystemType.Windows) 1 else 0)
    }
  }

}
