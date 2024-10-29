package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.Main
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, TypeRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends RubyCode2CpgFixture {

  "`x.y` is represented by a `x.y` field access" in {
    val cpg = code("""
        |x = Foo.new
        |x.y
        |""".stripMargin)

    inside(cpg.fieldAccess.code("x.y").headOption) {
      case Some(xyCall) =>
        xyCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        xyCall.name shouldBe Operators.fieldAccess
        xyCall.methodFullName shouldBe Operators.fieldAccess
        xyCall.lineNumber shouldBe Some(3)
        xyCall.code shouldBe "x.y"
      case None => fail("Expected a field access with the code `x.y`")
    }
  }

  "`self.x` should correctly create a `self` node field base" in {

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

    inside(cpg.fieldAccess.code("self.sick_days_earned").l) {
      case sickDays :: _ =>
        sickDays.code shouldBe "self.sick_days_earned"
        sickDays.name shouldBe Operators.fieldAccess
        sickDays.methodFullName shouldBe Operators.fieldAccess
        sickDays.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

        inside(sickDays.argument.l) {
          case (self: Identifier) :: (sickDaysId: FieldIdentifier) :: Nil =>
            self.name shouldBe "self"
            self.code shouldBe "self"
            self.typeFullName should endWith("PaidTimeOff")

            sickDaysId.canonicalName shouldBe "@sick_days_earned"
            sickDaysId.code shouldBe "sick_days_earned"
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
        |Base64::decode64() # self.Base64.decode64()
        |Baz::func1()       # self.Baz.func1()
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
          bazTypeRef.typeFullName shouldBe s"Test0.rb:$Main.Baz<class>"
          bazTypeRef.code shouldBe "module Baz (...)"

          val fooAssign = foo.argument(1).asInstanceOf[Call]
          fooAssign.name shouldBe Operators.fieldAccess
          fooAssign.code shouldBe "self.Foo"

          val fooTypeRef = foo.argument(2).asInstanceOf[TypeRef]
          fooTypeRef.typeFullName shouldBe s"Test0.rb:$Main.Foo<class>"
          fooTypeRef.code shouldBe "class Foo (...)"
        case _ => fail(s"Expected two type ref assignments on the module level")
      }
    }

    "give external type accesses on script-level the `self.` base" in {
      val call = cpg.method.isModule.call.nameExact("decode64").head
      call.name shouldBe "decode64"

      val base = call.argument(0).asInstanceOf[Identifier]
      base.code shouldBe "<tmp-1>"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "(<tmp-1> = Base64).decode64"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.assignment
      selfArg1.code shouldBe "<tmp-1> = Base64"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "decode64"
      selfArg2.code shouldBe "decode64"
    }

    "give internal type accesses on script-level the `self.` base" in {
      val call = cpg.method.isModule.call.nameExact("func1").head
      call.name shouldBe "func1"

      val base = call.argument(0).asInstanceOf[Identifier]
      base.code shouldBe "<tmp-2>"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "(<tmp-2> = Baz).func1"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.assignment
      selfArg1.code shouldBe "<tmp-2> = Baz"

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

      val base = call.argument(0).asInstanceOf[Identifier]
      base.code shouldBe "<tmp-3>"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "(<tmp-3> = Baz).func1"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.assignment
      selfArg1.code shouldBe "<tmp-3> = Baz"

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
        |      A::B::func()
        |    end
        |  end
        |end
        |
        |""".stripMargin)

    "create `TYPE_REF` targets for the field accesses" in {
      val call = cpg.call.nameExact("func").head
      val base = call.argument(0).asInstanceOf[Identifier]
      base.code shouldBe "<tmp-0>"

      val receiver = call.receiver.isCall.head
      receiver.name shouldBe Operators.fieldAccess
      receiver.code shouldBe "(<tmp-0> = A::B).func"

      val selfArg1 = receiver.argument(1).asInstanceOf[Call]
      selfArg1.name shouldBe Operators.assignment
      selfArg1.code shouldBe "<tmp-0> = A::B"

      selfArg1.argument(1).asInstanceOf[Identifier].code shouldBe s"<tmp-0>"

      val abRhs = selfArg1.argument(2).asInstanceOf[Call]
      abRhs.code shouldBe "A::B"

      abRhs.argument(1).asInstanceOf[TypeRef].typeFullName shouldBe s"Test0.rb:$Main.A<class>"
      abRhs.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "B"

      val selfArg2 = receiver.argument(2).asInstanceOf[FieldIdentifier]
      selfArg2.canonicalName shouldBe "func"
      selfArg2.code shouldBe "func"
    }
  }

}
