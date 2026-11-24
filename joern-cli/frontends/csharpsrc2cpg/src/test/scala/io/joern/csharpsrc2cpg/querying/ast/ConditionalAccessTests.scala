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
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Bar")).l) { case fieldAccess :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.referencedMember.l shouldBe cpg.member("Bar").l
      }
    }

    "assigned variable has correct properties" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).l) { case assign :: Nil =>
        assign.code shouldBe "x = this?.Bar"
        assign.typeFullName shouldBe "System.Int32"
        assign.target.start.isIdentifier.typeFullName.headOption shouldBe Some("System.Int32")
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
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Baz")).l) { case bazFieldAccess :: Nil =>
        bazFieldAccess.methodFullName shouldBe Operators.fieldAccess
        bazFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        bazFieldAccess.referencedMember.l shouldBe cpg.member("Baz").l

        inside(bazFieldAccess.arguments(1).isCall.isFieldAccess.l) { case barFieldAccess :: Nil =>
          barFieldAccess.methodFullName shouldBe Operators.fieldAccess
          barFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          barFieldAccess.referencedMember.l shouldBe cpg.member("Bar").l
        }
      }
    }

    "assigned variable has correct properties" in {
      inside(cpg.identifier.nameExact("x").argumentIndex(1).inCall.isAssignment.l) { case assign :: Nil =>
        assign.code shouldBe "x = this?.Bar?.Baz"
        assign.typeFullName shouldBe "Foo"
        assign.target.start.isIdentifier.typeFullName.headOption shouldBe Some("Foo")
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
      inside(cpg.call.nameExact("Bar").l) { case bar :: Nil =>
        bar.methodFullName shouldBe "Foo.Bar:System.Int32()"
        bar.receiver.l shouldBe bar.argument.argumentIndex(0).l
        bar.argument.argumentIndexGt(0) shouldBe empty
        inside(bar.argument(0)) { case thisArg: Identifier =>
          thisArg.code shouldBe "this"
          thisArg.typeFullName shouldBe "Foo"
        }
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
      inside(cpg.call.nameExact("Baz").l) { case baz :: Nil =>
        baz.methodFullName shouldBe "Foo.Baz:Foo()"
        baz.receiver.l shouldBe baz.argument.argumentIndex(0).l
        baz.argument.argumentIndexGt(0) shouldBe empty
        baz.argument(0).start.isCall.methodFullName.l shouldBe List("Foo.Bar:Foo()")
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
      inside(cpg.fieldAccess.where(_.fieldIdentifier.canonicalNameExact("Baz")).l) { case bazFieldAccess :: Nil =>
        bazFieldAccess.methodFullName shouldBe Operators.fieldAccess
        bazFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        bazFieldAccess.referencedMember.l shouldBe cpg.member("Baz").l

        inside(bazFieldAccess.arguments(1).isCall.isFieldAccess.l) { case barFieldAccess :: Nil =>
          barFieldAccess.methodFullName shouldBe Operators.fieldAccess
          barFieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          barFieldAccess.referencedMember.l shouldBe cpg.member("Bar").l
        }
      }
    }

    "assigned variable has correct properties" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).l) { case assign :: Nil =>
        assign.code shouldBe "x = this?.Bar.Baz"
        assign.typeFullName shouldBe "Foo"
        assign.target.start.isIdentifier.typeFullName.headOption shouldBe Some("Foo")
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
      inside(cpg.call.nameExact("Quux").l) { case quux :: Nil =>
        quux.methodFullName shouldBe "Foo.Quux:Foo()"
        quux.receiver.l shouldBe quux.argument.argumentIndex(0).l
        quux.argument.argumentIndexGt(0) shouldBe empty
        quux.argument(0) shouldBe cpg.call.nameExact("Baz").head
      }
    }

    "have correct properties and arguments to `Baz()`" in {
      inside(cpg.call.nameExact("Baz").l) { case baz :: Nil =>
        baz.methodFullName shouldBe "Foo.Baz:Foo()"
        baz.receiver.l shouldBe baz.argument.argumentIndex(0).l
        baz.argument.argumentIndexGt(0) shouldBe empty
        baz.argument(0) shouldBe cpg.call.nameExact("Bar").head
      }
    }

    "have correct properties and arguments to `Bar()`" in {
      inside(cpg.call.nameExact("Bar").l) { case bar :: Nil =>
        bar.methodFullName shouldBe "Foo.Bar:Foo(System.Int32)"
        bar.receiver.l shouldBe bar.argument.argumentIndex(0).l
        bar.argument.argumentIndexGt(0).size shouldBe 1
        bar.argument(1) shouldBe cpg.literal("0").head
        bar.argument(0) shouldBe cpg.identifier("this").head
      }
    }
  }

  "`n?.V = 3` (C# 14 null-conditional assignment)" should {
    val cpg = code("""
        |class Node
        |{
        | int V;
        | void DoStuff(Node n)
        | {
        |   n?.V = 3;
        | }
        |}
        |""".stripMargin)

    "be lowered as the assignment `n.V = 3`" in {
      inside(cpg.assignment.l) { case assign :: Nil =>
        assign.code shouldBe "n?.V = 3"
        assign.methodFullName shouldBe Operators.assignment
        assign.source.start.isLiteral.code.l shouldBe List("3")
        inside(assign.target.start.isCall.isFieldAccess.l) { case fieldAccess :: Nil =>
          fieldAccess.referencedMember.l shouldBe cpg.member("V").l
          fieldAccess.argument(1).start.isIdentifier.name.l shouldBe List("n")
        }
      }
    }
  }

  "`n?.Next?.V += 1` (C# 14 null-conditional compound assignment)" should {
    val cpg = code("""
        |class Node
        |{
        | Node Next;
        | int V;
        | void DoStuff(Node n)
        | {
        |   n?.Next?.V += 1;
        | }
        |}
        |""".stripMargin)

    "be lowered as the assignment `n.Next.V += 1`" in {
      inside(cpg.call.nameExact(Operators.assignmentPlus).l) { case assign :: Nil =>
        assign.code shouldBe "n?.Next?.V += 1"
        inside(assign.argument(1).start.isCall.isFieldAccess.l) { case vFieldAccess :: Nil =>
          vFieldAccess.referencedMember.l shouldBe cpg.member("V").l
          inside(vFieldAccess.argument(1).start.isCall.isFieldAccess.l) { case nextFieldAccess :: Nil =>
            nextFieldAccess.referencedMember.l shouldBe cpg.member("Next").l
            nextFieldAccess.argument(1).start.isIdentifier.name.l shouldBe List("n")
          }
        }
      }
    }
  }

  "`c?.P = 1` with `P` a property (C# 14 null-conditional assignment)" should {
    val cpg = code("""
        |class C
        |{
        | public int P { get; set; }
        | void DoStuff(C c)
        | {
        |   c?.P = 1;
        | }
        |}
        |""".stripMargin)

    "be lowered as the setter call `c.set_P(1)`" in {
      inside(cpg.call.nameExact("set_P").l) { case setter :: Nil =>
        setter.code shouldBe "c?.P = 1"
        setter.methodFullName shouldBe "C.set_P:System.Void(System.Int32)"
        setter.argument(0).start.isIdentifier.name.l shouldBe List("c")
        setter.argument(1).start.isLiteral.code.l shouldBe List("1")
      }
    }
  }

}
