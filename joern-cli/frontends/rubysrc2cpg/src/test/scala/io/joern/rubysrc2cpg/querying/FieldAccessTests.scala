package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, TypeRef}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends RubyCode2CpgFixture {

  "`x.y` is represented by an `x.y` CALL without arguments" in {
    val cpg = code("""
                     |x.y
                     |""".stripMargin)

    inside(cpg.call("y").headOption) {
      case Some(xyCall) =>
        xyCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        xyCall.lineNumber shouldBe Some(2)
        xyCall.code shouldBe "x.y"

        inside(xyCall.receiver.headOption) {
          case Some(x: Identifier) =>
            x.name shouldBe "x"
          case _ => fail("Expected an identifier receiver")
        }
      case None => fail("Expected a call with the name `y`")
    }
  }

  "`self.x` should correctly create a `this` node field base" in {

    // Example from railsgoat
    val cpg = code("""
        |class PaidTimeOff < ApplicationRecord
        |  belongs_to :user
        |  has_many :schedule, foreign_key: :user_id, primary_key: :user_id, dependent: :destroy
        |
        |  def sick_days_remaining
        |    self.sick_days_earned - self.sick_days_taken
        |  end
        |end
        |""".stripMargin)

    inside(cpg.call.name("sick_days_earned").l) {
      case sickDays :: _ =>
        sickDays.code shouldBe "self.sick_days_earned"
        sickDays.name shouldBe "sick_days_earned"
        sickDays.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

        inside(sickDays.argument.l) {
          case (self: Identifier) :: Nil =>
            self.name shouldBe "this"
            self.code shouldBe "self"
            self.typeFullName should endWith("PaidTimeOff")
          case xs => fail(s"Expected exactly two field access arguments, instead got [${xs.code.mkString(", ")}]")
        }
      case Nil => fail("Expected at least one call with `self` base, but got none.")
    }
  }

  "script-level type properties accessed at various lexical scopes" should {

    val cpg = code("""
        |require 'base64'
        |
        |# self.Baz = TYPE_REF Baz<class>
        |module Baz
        |  def func2()
        |  end
        |  def self.func1()
        |  end
        |end
        |
        |Base64::decode64 # self.Base64.decode64()
        |Baz::func1       # self.Baz.func1()
        |
        |# self.Foo = TYPE_REF Foo<class>
        |class Foo
        |  def func()
        |    Baz.func1()  # self.Baz.func1
        |  end
        |end
        |
        |f = Foo.new
        |f.func           # self.f.func
        |""".stripMargin)

    "assign an alias for type declarations to the singleton" in {
      inside(cpg.method.isModule.assignment.where(_.source.isTypeRef).l) {
        case baz :: foo :: Nil =>
          val bazAssign = baz.argument(1).asInstanceOf[Call]
          bazAssign.name shouldBe Operators.fieldAccess
          bazAssign.code shouldBe "self.Baz"

          val bazTypeRef = baz.argument(2).asInstanceOf[TypeRef]
          bazTypeRef.typeFullName shouldBe "Test0.rb:<global>::program.Baz<class>"
          bazTypeRef.code shouldBe "module Baz (...)"

          val fooAssign = foo.argument(1).asInstanceOf[Call]
          fooAssign.name shouldBe Operators.fieldAccess
          fooAssign.code shouldBe "self.Foo"

          val fooTypeRef = foo.argument(2).asInstanceOf[TypeRef]
          fooTypeRef.typeFullName shouldBe "Test0.rb:<global>::program.Foo<class>"
          fooTypeRef.code shouldBe "class Foo (...)"
        case _ => fail(s"Expected two type ref assignments on the module level")
      }
    }

    "give both internal and external type accesses on script-level the `self.` base" in {
      val externalCall = cpg.method.isModule.call.codeExact("Base64::decode64").head
//      val decodeReceiver = externalCall.receiver.isCall.head
      externalCall.dotAst.foreach(println)
      externalCall.astChildren.map(x => x.label -> x.code).foreach(println)
    }

  }

}
