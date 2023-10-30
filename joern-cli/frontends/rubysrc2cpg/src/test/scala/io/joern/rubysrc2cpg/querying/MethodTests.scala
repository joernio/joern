package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodTests extends RubyCode2CpgFixture {

  "`def f(x) = 1` is represented by a METHOD node" in {
    val cpg = code("""
                     |def f(x) = 1
                     |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.name shouldBe "f"
    f.fullName shouldBe "Test0.rb:<global>::program:f"
    f.isExternal shouldBe false
    f.lineNumber shouldBe Some(2)
    f.numberOfLines shouldBe 1

    val List(x) = f.parameter.name("x").l
    x.index shouldBe 0
    x.isVariadic shouldBe false
    x.lineNumber shouldBe Some(2)
  }

  "`def f ... return 1 ... end` is represented by a METHOD node" in {
    val cpg = code("""
                     |def f
                     | return 1
                     |end
                     |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.name shouldBe "f"
    f.fullName shouldBe "Test0.rb:<global>::program:f"
    f.isExternal shouldBe false
    f.lineNumber shouldBe Some(2)
    f.numberOfLines shouldBe 3
    f.parameter.size shouldBe 0
  }

}
