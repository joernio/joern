package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language.*

class SetterTests extends RubyCode2CpgFixture {

  "`x.y=1` is approximated by a `x.y =` assigment with argument `1`" in {
    val cpg = code("""x = Foo.new
                     |x.y = 1
                     |""".stripMargin)
    inside(cpg.assignment.where(_.source.isLiteral.codeExact("1")).l) {
      case xyAssign :: Nil =>
        xyAssign.lineNumber shouldBe Some(2)
        xyAssign.code shouldBe "x.y = 1"

        val fieldTarget = xyAssign.target.asInstanceOf[Call]
        fieldTarget.code shouldBe "x.@y"
        fieldTarget.name shouldBe Operators.fieldAccess

        inside(fieldTarget.argument.l) {
          case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
            base.name shouldBe "x"
            field.canonicalName shouldBe "@y"
          case xs => fail("Expected field access to have two targets")
        }
      case xs => fail("Expected a single assignment to the literal `1`")
    }
  }

}
