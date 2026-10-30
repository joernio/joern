package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class ConstructorChainInitializerTests extends C2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  implicit val resolver: NoResolve.type = NoResolve

  "a constructor with a base-class and a data member initializer" should {
    val cpg = code("""
        |struct Base {
        |  int a;
        |  int b;
        |  Base(int x, int y) : a(x), b(y) {}
        |};
        |struct Derived : Base {
        |  int c;
        |  Derived(int x, int y) : c(x), Base(x, y) { c = c + 1; }
        |};
        |""".stripMargin)

    "call the base constructor with all of its arguments" in {
      inside(cpg.method.fullNameExact("Derived.Derived:void(int,int)").call.methodFullName("Base.*").l) {
        case List(baseConstructorCall) =>
          baseConstructorCall.name shouldBe "Base"
          baseConstructorCall.methodFullName shouldBe "Base.Base:void(int,int)"
          baseConstructorCall.signature shouldBe "void(int,int)"
          baseConstructorCall.code shouldBe "Base(x, y)"
          inside(baseConstructorCall.argument(1)) { case x: Identifier => x.name shouldBe "x" }
          inside(baseConstructorCall.argument(2)) { case y: Identifier => y.name shouldBe "y" }
      }
    }

    "construct into `this` instead of into a temporary" in {
      inside(cpg.call.methodFullNameExact("Base.Base:void(int,int)").l) { case List(baseConstructorCall) =>
        inside(baseConstructorCall.argument(0)) { case receiver: Identifier =>
          receiver.name shouldBe "this"
          receiver.typeFullName shouldBe "Derived*"
        }
      }
      cpg.method.fullNameExact("Derived.Derived:void(int,int)").call.nameExact(Operators.alloc) shouldBe empty
      cpg.method.fullNameExact("Derived.Derived:void(int,int)").local.name("<tmp>.*") shouldBe empty
    }

    "give the base constructor a caller" in {
      cpg.method.fullNameExact("Base.Base:void(int,int)").callIn.size shouldBe 1
    }

    "not initialize a data member named after the base class" in {
      cpg.fieldIdentifier.canonicalNameExact("Base") shouldBe empty
      cpg.assignment.code("this->Base.*") shouldBe empty
    }

    "initialize before the body runs" in {
      val constructorBody     = cpg.method.fullNameExact("Derived.Derived:void(int,int)").body.head
      val initializers        = constructorBody.astChildren.isCall.filterNot(_.code == "c = c + 1").l
      val List(bodyStatement) = constructorBody.astChildren.isCall.codeExact("c = c + 1").l
      initializers.size shouldBe 2
      initializers.map(_.order).max should be < bodyStatement.order
    }

    "keep the plain data member initializer an assignment" in {
      inside(cpg.assignment.codeExact("this->c = x").l) { case List(assignment) =>
        inside(assignment.argument(1)) { case target: Call =>
          target.name shouldBe Operators.indirectFieldAccess
          inside(target.argument(2)) { case member: FieldIdentifier => member.canonicalName shouldBe "c" }
        }
        inside(assignment.argument(2)) { case source: Identifier => source.name shouldBe "x" }
      }
    }

    "type a member read through `this` with the type of the member" in {
      // the member initializer and the body access the very same member
      cpg.call
        .nameExact(Operators.indirectFieldAccess)
        .codeExact("this->c")
        .typeFullName
        .l
        .distinct shouldBe List("int")
    }
  }

  "a delegating constructor" should {
    val cpg = code("""
        |struct Derived {
        |  int c;
        |  Derived(int x, int y) : c(x) {}
        |  Derived() : Derived(1, 2) {}
        |};
        |""".stripMargin)

    "call the delegated-to constructor with all of its arguments" in {
      inside(cpg.method.fullNameExact("Derived.Derived:void()").call.methodFullName("Derived.*").l) {
        case List(delegatingCall) =>
          delegatingCall.methodFullName shouldBe "Derived.Derived:void(int,int)"
          delegatingCall.code shouldBe "Derived(1, 2)"
          inside(delegatingCall.argument(0)) { case receiver: Identifier => receiver.name shouldBe "this" }
          inside(delegatingCall.argument(1)) { case one: Literal => one.code shouldBe "1" }
          inside(delegatingCall.argument(2)) { case two: Literal => two.code shouldBe "2" }
      }
      cpg.method.fullNameExact("Derived.Derived:void(int,int)").callIn.size shouldBe 1
    }

    "not initialize a data member named after the enclosing class" in {
      cpg.fieldIdentifier.canonicalNameExact("Derived") shouldBe empty
      cpg.assignment.code("this->Derived.*") shouldBe empty
    }
  }

  "a qualified base-class initializer" should {
    val cpg = code("""
        |namespace ns {
        |struct P {
        |  int p;
        |  P(int u, int v) : p(u) {}
        |};
        |} // namespace ns
        |struct Q : ns::P {
        |  Q(int u, int v) : ns::P(u, v) {}
        |};
        |""".stripMargin)

    "keep resolving to the base constructor with all of its arguments" in {
      inside(cpg.method.fullNameExact("Q.Q:void(int,int)").call.methodFullName("ns.*").l) { case List(baseCall) =>
        baseCall.name shouldBe "ns.P"
        baseCall.methodFullName shouldBe "ns.P.P:void(int,int)"
        baseCall.code shouldBe "ns::P(u, v)"
        inside(baseCall.argument(1)) { case u: Identifier => u.name shouldBe "u" }
        inside(baseCall.argument(2)) { case v: Identifier => v.name shouldBe "v" }
      }
      cpg.method.fullNameExact("ns.P.P:void(int,int)").callIn.size shouldBe 1
    }
  }

  "a data member of class type" should {
    val cpg = code("""
        |struct Base {
        |  int a;
        |  Base(int x, int y) : a(x) {}
        |};
        |struct Holder {
        |  Base m;
        |  Holder(int x, int y) : m(x, y) {}
        |};
        |""".stripMargin)

    "stay an assignment, as it is not a base-class initializer" in {
      cpg.method.fullNameExact("Holder.Holder:void(int,int)").call.methodFullName("Base.*") shouldBe empty
      inside(cpg.method.fullNameExact("Holder.Holder:void(int,int)").call.isAssignment.l) { case List(assignment) =>
        inside(assignment.argument(1)) { case target: Call =>
          target.name shouldBe Operators.indirectFieldAccess
          inside(target.argument(2)) { case member: FieldIdentifier => member.canonicalName shouldBe "m" }
        }
      }
    }
  }

}
