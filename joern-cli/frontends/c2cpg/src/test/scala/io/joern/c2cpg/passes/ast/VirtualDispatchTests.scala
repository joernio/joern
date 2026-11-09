package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class VirtualDispatchTests extends C2CpgSuite(FileDefaults.CppExt) {

  implicit val resolver: NoResolve.type = NoResolve

  "A member function of a class that also has non-virtual members" should {
    val cpg = code("""
        |struct Base {
        |  virtual void keywordKept();
        |  virtual void overrideOnly();
        |  virtual void finalOnly();
        |  virtual void noSpecifier();
        |  virtual void hidden(int);
        |  void notVirtual();
        |};
        |struct Mid : Base {
        |  virtual void keywordKept();
        |  void overrideOnly() override;
        |  void finalOnly() final;
        |  void noSpecifier();
        |  void hidden(double);
        |  void notVirtual();
        |};
        |void Base::keywordKept() {}
        |void Base::overrideOnly() {}
        |void Base::finalOnly() {}
        |void Base::noSpecifier() {}
        |void Base::hidden(int) {}
        |void Base::notVirtual() {}
        |void Mid::keywordKept() {}
        |void Mid::overrideOnly() {}
        |void Mid::finalOnly() {}
        |void Mid::noSpecifier() {}
        |void Mid::hidden(double) {}
        |void Mid::notVirtual() {}
        |void callSites(Base* b, Mid* m) {
        |  b->keywordKept();
        |  m->keywordKept();
        |  m->overrideOnly();
        |  m->finalOnly();
        |  m->noSpecifier();
        |  m->hidden(1.0);
        |  m->notVirtual();
        |}
        |""".stripMargin)

    "dispatch dynamically when the keyword is spelled" in {
      inside(cpg.call.codeExact("m->keywordKept()").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.keywordKept:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        call.receiver.code.l shouldBe List("m")
      }
    }

    "dispatch dynamically for an override without the keyword" in {
      inside(cpg.call.codeExact("m->overrideOnly()").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.overrideOnly:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        call.receiver.code.l shouldBe List("m")
        call.callee.fullName.l shouldBe List("Mid.overrideOnly:void()")
      }
    }

    "dispatch dynamically for an override declared final" in {
      inside(cpg.call.codeExact("m->finalOnly()").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.finalOnly:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        call.receiver.code.l shouldBe List("m")
      }
    }

    "dispatch dynamically for an override with no specifier" in {
      inside(cpg.call.codeExact("m->noSpecifier()").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.noSpecifier:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        call.receiver.code.l shouldBe List("m")
      }
    }

    "dispatch dynamically through a base class pointer" in {
      inside(cpg.call.codeExact("b->keywordKept()").l) { case List(call) =>
        call.methodFullName shouldBe "Base.keywordKept:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      }
    }

    "dispatch statically for a member that hides rather than overrides" in {
      inside(cpg.call.codeExact("m->hidden(1.0)").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.hidden:void(double)"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "dispatch statically for a non-virtual member of the same class" in {
      inside(cpg.call.codeExact("m->notVirtual()").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.notVirtual:void()"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        call.callee.fullName.l shouldBe List("Mid.notVirtual:void()")
      }
    }
  }

  "An override two levels below the keyword" should {
    val cpg = code("""
        |struct Root {
        |  virtual void deep();
        |};
        |struct Middle : Root {
        |  void deep() override;
        |};
        |struct Leaf : Middle {
        |  void deep() override;
        |};
        |void Root::deep() {}
        |void Middle::deep() {}
        |void Leaf::deep() {}
        |void callSite(Leaf* l) { l->deep(); }
        |""".stripMargin)

    "dispatch dynamically" in {
      inside(cpg.call.codeExact("l->deep()").l) { case List(call) =>
        call.methodFullName shouldBe "Leaf.deep:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      }
    }
  }

  "A virtual call operator" should {
    val cpg = code("""
        |struct Base {
        |  virtual int operator()(int);
        |};
        |struct Mid : Base {
        |  int operator()(int) override;
        |};
        |struct Plain {
        |  int operator()(int);
        |};
        |void callSites(Mid& mid, Plain& plain) {
        |  mid(1);
        |  plain(1);
        |}
        |""".stripMargin)

    "dispatch dynamically for an override without the keyword" in {
      inside(cpg.call.codeExact("mid(1)").l) { case List(call) =>
        call.methodFullName shouldBe "Mid.():int(int)"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      }
    }

    "dispatch statically when the call operator is not virtual" in {
      inside(cpg.call.codeExact("plain(1)").l) { case List(call) =>
        call.methodFullName shouldBe "Plain.():int(int)"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }
  }

  "A pure virtual member function" should {
    val cpg = code("""
        |struct Abstract {
        |  virtual void withKeyword() = 0;
        |  virtual void reDeclared() = 0;
        |};
        |struct Impl : Abstract {
        |  void reDeclared() = 0;
        |};
        |void callSites(Abstract* a, Impl* i) {
        |  a->withKeyword();
        |  i->reDeclared();
        |}
        |""".stripMargin)

    "dispatch dynamically in both spellings" in {
      inside(cpg.call.codeExact("a->withKeyword()").l) { case List(call) =>
        call.methodFullName shouldBe "Abstract.withKeyword:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      }
      inside(cpg.call.codeExact("i->reDeclared()").l) { case List(call) =>
        call.methodFullName shouldBe "Impl.reDeclared:void()"
        call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      }
    }
  }

  "A member function of a class with no virtual members at all" should {
    val cpg = code("""
        |struct Plain {
        |  void ordinary();
        |};
        |struct Derived : Plain {
        |  void alsoOrdinary();
        |};
        |void callSites(Plain* p, Derived* d) {
        |  p->ordinary();
        |  d->alsoOrdinary();
        |}
        |""".stripMargin)

    "dispatch statically" in {
      inside(cpg.call.codeExact("p->ordinary()").l) { case List(call) =>
        call.methodFullName shouldBe "Plain.ordinary:void()"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        call.receiver.l shouldBe empty
      }
      inside(cpg.call.codeExact("d->alsoOrdinary()").l) { case List(call) =>
        call.methodFullName shouldBe "Derived.alsoOrdinary:void()"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }
  }
}
