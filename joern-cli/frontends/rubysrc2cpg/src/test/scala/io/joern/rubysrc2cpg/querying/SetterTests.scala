package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class SetterTests extends RubyCode2CpgFixture {

  "`x.y=1` is represented by a `x.y=` CALL with argument `1`" in {
    val cpg = code("""x = Foo.new
                     |x.y = 1
                     |""".stripMargin)

    val List(setter)      = cpg.call("y=").l
    val List(fieldAccess) = cpg.fieldAccess.l

    setter.code shouldBe "x.y = 1"
    setter.lineNumber shouldBe Some(2)
    setter.receiver.l shouldBe List(fieldAccess)

    fieldAccess.code shouldBe "x.y="
    fieldAccess.lineNumber shouldBe Some(2)
    fieldAccess.fieldIdentifier.code.l shouldBe List("y=")

    val List(_, one) = setter.argument.l
    one.code shouldBe "1"
    one.lineNumber shouldBe Some(2)
  }

}
