package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ConditionalAccessTests extends CSharpCode2CpgFixture {

  "`this?.Bar` assigned to a variable" should {
    val cpg = code("""
        |class Foo
        |{
        | int Bar = 1;
        | void DoStuff()
        | {
        |   var x = this?.Bar;
        | }
        |}
        |
        |""".stripMargin)

    "be lowered as a field access `this.Bar`" in {
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Bar")).l) {
        case fieldAccess :: Nil =>
          fieldAccess.methodFullName shouldBe Operators.fieldAccess
          fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          fieldAccess.referencedMember.l shouldBe cpg.member("Bar").l
        case _ => fail(s"Expected single field access to `Bar`")
      }
    }

    "assigned variable has correct properties" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).l) {
        case assign :: Nil =>
          assign.code shouldBe "x = this?.Bar"
          assign.typeFullName shouldBe "System.Int32"
          assign.target.start.isIdentifier.typeFullName.headOption shouldBe Some("System.Int32")
        case _ => fail(s"Expected single assignment to `x`")
      }
    }
  }

  "`this?.Bar?.Baz` assigned to a variable" should {
    val cpg = code("""
        |class Foo
        |{
        | Foo Bar;
        | Foo Baz;
        | void DoStuff()
        | {
        |   var x = this?.Bar?.Baz;
        | }
        |}
        |
        |""".stripMargin)

    "be lowered as a field access `this.Bar.Baz`" in {
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Baz")).l) {
        case bazFieldAccess :: Nil =>
          bazFieldAccess.methodFullName shouldBe Operators.fieldAccess
          bazFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          bazFieldAccess.referencedMember.l shouldBe cpg.member("Baz").l

          inside(bazFieldAccess.start.argument(1).fieldAccess.l) {
            case barFieldAccess :: Nil =>
              barFieldAccess.methodFullName shouldBe Operators.fieldAccess
              barFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
              barFieldAccess.referencedMember.l shouldBe cpg.member("Bar").l
            case _ => fail(s"Expected single field access to `Baz`")
          }
        case _ => fail(s"Expected single field access to `Bar`")
      }
    }

    "assigned variable has correct properties" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).l) {
        case assign :: Nil =>
          assign.code shouldBe "x = this?.Bar?.Baz"
          assign.typeFullName shouldBe "Foo"
          assign.target.start.isIdentifier.typeFullName.headOption shouldBe Some("Foo")
        case _ => fail(s"Expected single assignment to `x`")
      }
    }
  }

  "`this?.Bar()`" should {
    val cpg = code("""
        |class Foo
        |{
        | int Bar() { return 1; }
        | void DoStuff()
        | {
        |   this?.Bar();
        | }
        |}
        |""".stripMargin)

    "be lowered as a call to `Bar()` with receiver `this`" in {
      inside(cpg.call.nameExact("Bar").l) {
        case bar :: Nil =>
          bar.methodFullName shouldBe "Foo.Bar:System.Int32()"
          bar.receiver.l shouldBe bar.argument.argumentIndex(0).l
          bar.argument.argumentIndexGt(0) shouldBe empty
          inside(bar.argument(0)) {
            case thisArg: Identifier =>
              thisArg.code shouldBe "this"
              thisArg.typeFullName shouldBe "Foo"
            case xs => fail(s"Expected single identifier argument to Bar, but got $xs")
          }
        case xs => fail(s"Expected single call to Bar, but got $xs")
      }
    }
  }

  "`this?.Bar()?.Baz()`" should {
    val cpg = code("""
        |class Foo
        |{
        | Foo Bar() { return null; }
        | Foo Baz() { return null; }
        | void DoStuff()
        | {
        |   this?.Bar()?.Baz();
        | }
        |}
        |""".stripMargin)

    "have correct properties and arguments to `Baz()`" in {
      inside(cpg.call.nameExact("Baz").l) {
        case baz :: Nil =>
          baz.methodFullName shouldBe "Foo.Baz:Foo()"
          baz.receiver.l shouldBe baz.argument.argumentIndex(0).l
          baz.argument.argumentIndexGt(0) shouldBe empty
          baz.argument(0).start.isCall.methodFullName.l shouldBe List("Foo.Bar:Foo()")
        case xs => fail(s"Expected single call to Baz, but got $xs")
      }
    }
  }

  "`this?.Bar.Baz` assigned to a variable" should {
    val cpg = code("""
        |class Foo
        |{
        | Foo Bar;
        | Foo Baz;
        | void DoStuff()
        | {
        |   var x = this?.Bar.Baz;
        | }
        |}
        |""".stripMargin)

    "be lowered as a field access `this.Bar.Baz`" in {
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Baz")).l) {
        case bazFieldAccess :: Nil =>
          bazFieldAccess.methodFullName shouldBe Operators.fieldAccess
          bazFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          bazFieldAccess.referencedMember.l shouldBe cpg.member("Baz").l

          inside(bazFieldAccess.start.argument(1).fieldAccess.l) {
            case barFieldAccess :: Nil =>
              barFieldAccess.methodFullName shouldBe Operators.fieldAccess
              barFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
              barFieldAccess.referencedMember.l shouldBe cpg.member("Bar").l
            case _ => fail(s"Expected single field access to `Baz`")
          }
        case _ => fail(s"Expected single field access to `Bar`")
      }
    }

    "assigned variable has correct properties" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).l) {
        case assign :: Nil =>
          assign.code shouldBe "x = this?.Bar.Baz"
          assign.typeFullName shouldBe "Foo"
          assign.target.start.isIdentifier.typeFullName.headOption shouldBe Some("Foo")
        case _ => fail(s"Expected single assignment to `x`")
      }
    }
  }

  "`this?.Bar(0)?.Baz()?.Quux()`" should {
    val cpg = code("""
        |class Foo
        |{
        | Foo Bar(int x) { return null; }
        | Foo Baz() { return null; }
        | Foo Quux() { return null; }
        | void DoStuff()
        | {
        |   this?.Bar(0)?.Baz()?.Quux();
        | }
        |}
        |
        |""".stripMargin)

    "have correct properties and arguments to `Quux()`" in {
      inside(cpg.call.nameExact("Quux").l) {
        case quux :: Nil =>
          quux.methodFullName shouldBe "Foo.Quux:Foo()"
          quux.receiver.l shouldBe quux.argument.argumentIndex(0).l
          quux.argument.argumentIndexGt(0) shouldBe empty
          quux.argument(0) shouldBe cpg.call.nameExact("Baz").head
        case _ => fail("Expected single call to `Quux`")
      }
    }

    "have correct properties and arguments to `Baz()`" in {
      inside(cpg.call.nameExact("Baz").l) {
        case baz :: Nil =>
          baz.methodFullName shouldBe "Foo.Baz:Foo()"
          baz.receiver.l shouldBe baz.argument.argumentIndex(0).l
          baz.argument.argumentIndexGt(0) shouldBe empty
          baz.argument(0) shouldBe cpg.call.nameExact("Bar").head
        case _ => fail("Expected single call to `Baz`")
      }
    }

    "have correct properties and arguments to `Bar()`" in {
      inside(cpg.call.nameExact("Bar").l) {
        case bar :: Nil =>
          bar.methodFullName shouldBe "Foo.Bar:Foo(System.Int32)"
          bar.receiver.l shouldBe bar.argument.argumentIndex(0).l
          bar.argument.argumentIndexGt(0).size shouldBe 1
          bar.argument(1) shouldBe cpg.literal("0").head
          bar.argument(0) shouldBe cpg.identifier("this").head
        case _ => fail("Expected single call to `Bar`")
      }
    }
  }

}
