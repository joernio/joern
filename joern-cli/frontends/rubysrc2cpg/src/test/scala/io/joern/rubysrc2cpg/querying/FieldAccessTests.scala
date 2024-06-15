package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, TypeRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
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

        inside(xyCall.argumentOption(0)) {
          case Some(receiver: Call) =>
            receiver.name shouldBe Operators.fieldAccess
            receiver.code shouldBe "self.x"
          case _ => fail("Expected an field access receiver")
        }

        inside(xyCall.receiver.headOption) {
          case Some(xyBase: Call) =>
            xyBase.name shouldBe Operators.fieldAccess
            xyBase.code shouldBe "x.y"

            val selfX = xyBase.argument(1).asInstanceOf[Call]
            selfX.code shouldBe "self.x"

            val yIdentifier = xyBase.argument(2).asInstanceOf[FieldIdentifier]
            yIdentifier.code shouldBe "y"
          case _ => fail("Expected an field access receiver")
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
            self.name shouldBe "self"
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
        |f.func()           # self.f.func
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

    "give external type accesses on script-level the `self.` base" in {
      val call = cpg.method.isModule.call.codeExact("Base64::decode64").head
      call.name shouldBe "decode64"

      val base = call.argument(0).asInstanceOf[Call]
      base.name shouldBe Operators.fieldAccess
      base.code shouldBe "self.Base64"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "Base64.decode64"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.fieldAccess
      selfArg1.code shouldBe "self.Base64"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "decode64"
      selfArg2.code shouldBe "decode64"
    }

    "give internal type accesses on script-level the `self.` base" in {
      val call = cpg.method.isModule.call.codeExact("Baz::func1").head
      call.name shouldBe "func1"

      val base = call.argument(0).asInstanceOf[Call]
      base.name shouldBe Operators.fieldAccess
      base.code shouldBe "self.Baz"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "Baz.func1"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.fieldAccess
      selfArg1.code shouldBe "self.Baz"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "func1"
      selfArg2.code shouldBe "func1"
    }

    "give method call accesses on script-level the `self.` base" in {
      val call = cpg.method.isModule.call.nameExact("func").head
      call.name shouldBe "func"

      val base = call.argument(0).asInstanceOf[Identifier]
      base.name shouldBe "f"
      base.code shouldBe "f"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "f.func"

      val selfArg1 = receiver.argument(1).asInstanceOf[Identifier]
      selfArg1.name shouldBe "f"
      selfArg1.code shouldBe "f"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "func"
      selfArg2.code shouldBe "func"
    }

    "give method call accesses inside of a class method the `self.` base if the type referred to is from `self`" in {
      val call = cpg.method.nameExact("func").call.nameExact("func1").head
      call.name shouldBe "func1"

      val base = call.argument(0).asInstanceOf[Call]
      base.name shouldBe Operators.fieldAccess
      base.code shouldBe "self.Baz"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "Baz.func1"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.fieldAccess
      selfArg1.code shouldBe "self.Baz"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "func1"
      selfArg2.code shouldBe "func1"
    }

  }

  "a member access call referring to a parent class in the lexical scope" should {
    val cpg = code("""
        |module A
        |  module B
        |    def func()
        |    end
        |    module C
        |      # TYPE_REF A <fieldAccess> B <fieldAccess> func
        |      A::B::func
        |    end
        |  end
        |end
        |
        |""".stripMargin)

    "create `TYPE_REF` targets for the field accesses" in {
      val call = cpg.call.nameExact("func").head
      val base = call.argument(0).asInstanceOf[Call]
      base.name shouldBe Operators.fieldAccess
      base.code shouldBe "A::B"

      base.argument(1).asInstanceOf[TypeRef].typeFullName shouldBe "Test0.rb:<global>::program.A<class>"
      base.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "B"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "A::B.func"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.fieldAccess
      selfArg1.code shouldBe "A::B"

      selfArg1.argument(1).asInstanceOf[TypeRef].typeFullName shouldBe "Test0.rb:<global>::program.A<class>"
      selfArg1.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "B"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "func"
      selfArg2.code shouldBe "func"
    }
  }

}
