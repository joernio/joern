package io.joern.rubysrc2cpg.querying

import io.joern.x2cpg.Defines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{Operators, DispatchTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language.*

class AttributeAccessorTests extends RubyCode2CpgFixture {

  "`x.y=1` is approximated by a `x.y =` assignment with argument `1`" in {
    val cpg = code("""x = Foo.new
                     |x.y = 1
                     |""".stripMargin)

    inside(cpg.assignment.where(_.source.isLiteral.codeExact("1")).l) {
      case xyAssign :: Nil =>
        xyAssign.lineNumber shouldBe Some(2)
        xyAssign.code shouldBe "x.y = 1"

        val fieldTarget = xyAssign.target.asInstanceOf[Call]
        fieldTarget.code shouldBe "x.y"
        fieldTarget.name shouldBe Operators.fieldAccess
        fieldTarget.methodFullName shouldBe Operators.fieldAccess
        fieldTarget.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

        inside(fieldTarget.argument.l) {
          case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
            base.name shouldBe "x"
            field.canonicalName shouldBe "@y"
            field.code shouldBe "y"
          case xs => fail("Expected field access to have two targets")
        }
      case xs => fail("Expected a single assignment to the literal `1`")
    }
  }

  "`x.y` is represented by a field access `x.y`" in {
    val cpg = code("""x = Foo.new
                     |a = x.y
                     |b = x.z()
                     |""".stripMargin)
    // Test the field access
    inside(cpg.fieldAccess.lineNumber(2).codeExact("x.y").l) {
      case xyCall :: Nil =>
        xyCall.lineNumber shouldBe Some(2)
        xyCall.code shouldBe "x.y"
        xyCall.methodFullName shouldBe Operators.fieldAccess
        xyCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      case xs => fail("Expected a single field access for `x.y`")
    }
    // Test an explicit call with parenthesis
    inside(cpg.call("z").lineNumber(3).l) {
      case xzCall :: Nil =>
        xzCall.lineNumber shouldBe Some(3)
        xzCall.code shouldBe "x.z()"
        xzCall.methodFullName shouldBe Defines.DynamicCallUnknownFullName
        xzCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      case xs => fail("Expected a single call for `x.z()`")
    }
  }

}
