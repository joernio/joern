package io.joern.c2cpg.passes

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FunctionDeclNodePassTests extends C2CpgSuite {

  "the FunctionDeclNodePass" should {
    "create proper bindings for forward-declared C++ functions" in {
      val cpg = code(
        """
          |class Foo {
          |  public:
          |    int fooA(int a); // forward-declared
          |    int fooB(int a) { return 0; } // implemented right away
          |    int fooC(int a); // not implemented at all
          |}
          |""".stripMargin,
        "main.hpp"
      ).moreCode(
        """
          |#include "main.hpp"
          |
          |int Foo::fooA(int a) { return 0; }
          |""".stripMargin,
        "main.cpp"
      )
      cpg.typeDecl.nameExact("Foo").methodBinding.sortBy(_.methodFullName).l.map { b =>
        (b.methodFullName, b.name, b.signature)
      } shouldBe List(
        ("Foo.fooA:int(int)", "fooA", "int(int)"),
        ("Foo.fooB:int(int)", "fooB", "int(int)"),
        ("Foo.fooC:int(int)", "fooC", "int(int)")
      )
      val List(fooA) = cpg.method.nameExact("fooA").l
      fooA.fullName shouldBe "Foo.fooA:int(int)"
      cpg.typeDecl.nameExact("Foo").methodBinding.methodFullNameExact("Foo.fooA:int(int)").boundMethod.l shouldBe List(
        fooA
      )
      cpg.typeDecl
        .fullNameExact("Foo.fooA:int(int)")
        .methodBinding
        .methodFullNameExact("Foo.fooA:int(int)")
        .boundMethod
        .l shouldBe List(fooA)

      val List(fooB) = cpg.method.nameExact("fooB").l
      fooB.fullName shouldBe "Foo.fooB:int(int)"
      cpg.typeDecl.nameExact("Foo").methodBinding.methodFullNameExact("Foo.fooB:int(int)").boundMethod.l shouldBe List(
        fooB
      )

      val List(fooC) = cpg.method.nameExact("fooC").l
      fooC.fullName shouldBe "Foo.fooC:int(int)"
      cpg.typeDecl
        .nameExact("Foo")
        .methodBinding
        .methodFullNameExact("Foo.fooC:int(int)")
        .boundMethod
        .l shouldBe List(fooC)
    }

    "create proper bindings for forward-declared C++ functions with templates" in {
      val cpg = code(
        """
          |class Foo {
          |  public:
          |    template<typename BarA>
          |    int fooA(int a); // forward-declared
          |
          |    template<typename BarB, Kind a>
          |    int fooA(int a) { return 0; } // implemented right away
          |
          |    template<typename BarC, Kind a, Kind b>
          |    int fooC(int a); // not implemented at all
          |}
          |""".stripMargin,
        "main.hpp"
      ).moreCode(
        """
          |#include "main.hpp"
          |
          |template<typename BarA>
          |int Foo::fooA(int a) { return 0; }
          |""".stripMargin,
        "main.cpp"
      )
      cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).fullName.sorted.l shouldBe List(
        "ANY",
        "Foo",
        "Foo*",
        "Foo.fooA:int(int)",
        "int",
        "void"
      )
      cpg.typeDecl.nameExact("Foo").methodBinding.sortBy(_.methodFullName).l.map { b =>
        (b.methodFullName, b.name, b.signature)
      } shouldBe List(
        ("Foo.fooA:int(int)", "fooA", "int(int)"),
        ("Foo.fooA<duplicate>0:int(int)", "fooA<duplicate>0", "int(int)"),
        ("Foo.fooC:int(int)", "fooC", "int(int)")
      )
      val List(fooA) = cpg.method.fullNameExact("Foo.fooA:int(int)").l
      fooA.file.name.l shouldBe List("main.cpp")
      fooA.code shouldBe "int Foo::fooA(int a) { return 0; }"
      cpg.typeDecl.nameExact("Foo").methodBinding.methodFullNameExact("Foo.fooA:int(int)").boundMethod.l shouldBe List(
        fooA
      )

      val List(fooADuplicate) = cpg.method.fullNameExact("Foo.fooA<duplicate>0:int(int)").l
      fooADuplicate.file.name.l shouldBe List("main.hpp")
      fooADuplicate.code shouldBe "int fooA(int a) { return 0; }"
      cpg.typeDecl
        .nameExact("Foo")
        .methodBinding
        .methodFullNameExact("Foo.fooA<duplicate>0:int(int)")
        .boundMethod
        .l shouldBe List(fooADuplicate)

      val List(fooC) = cpg.method.nameExact("fooC").l
      fooC.fullName shouldBe "Foo.fooC:int(int)"
      cpg.typeDecl.nameExact("Foo").methodBinding.methodFullNameExact("Foo.fooC:int(int)").boundMethod.l shouldBe List(
        fooC
      )
    }
  }

}
