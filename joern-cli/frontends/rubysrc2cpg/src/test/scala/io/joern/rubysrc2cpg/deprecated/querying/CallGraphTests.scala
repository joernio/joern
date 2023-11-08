package io.joern.rubysrc2cpg.deprecated.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

class CallGraphTests extends RubyCode2CpgFixture(withPostProcessing = true, useDeprecatedFrontend = true) {

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
    callToBar.methodFullName shouldBe "Test0.rb::program.bar"
    callToBar.lineNumber shouldBe Some(7)
    val List(bar: Method) = cpg.method("bar").internal.l
    bar.fullName shouldBe callToBar.methodFullName
    bar.caller.name.l shouldBe List("foo")
  }

}
