package io.joern.rubysrc2cpg.querying

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import scala.reflect.ClassTag
import io.joern.rubysrc2cpg.passes.Defines.RubyOperators

class RegexTests extends RubyCode2CpgFixture {
  "`'x' =~ y` is a member call `'x'.=~ /y/" in {
    val cpg = code("""|'x' =~ /y/
       |0
       |""".stripMargin)
    cpg.call(RubyOperators.regexpMatch).methodFullName.l shouldBe List(s"__builtin.String:${RubyOperators.regexpMatch}")
  }
  "`/x/ =~ 'y'` is a member call `/x/.=~ 'y'" in {
    val cpg = code("""|/x/ =~ 'y'
       |0
       |""".stripMargin)
    cpg.call(RubyOperators.regexpMatch).methodFullName.l shouldBe List(s"__builtin.Regexp:${RubyOperators.regexpMatch}")
  }
}
