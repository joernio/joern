package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class CallGraphTests extends RubyCode2CpgFixture {

  val cpg = code("""
      |def bar(content)
      |puts content
      |end
      |
      |def foo
      |bar( 1 )
      |end
      |""".stripMargin)

  "should identify call from `foo` to `bar`" in {
    val List(callToBar) = cpg.call("bar").l
    callToBar.name shouldBe "bar"
    callToBar.lineNumber shouldBe Some(7)
    cpg.method("bar").caller.name.l shouldBe List("foo")
  }

}
