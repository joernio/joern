package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class MemberPointerRefTests extends C2CpgSuite(FileDefaults.CppExt) {

  "A reference to a const member function" should {

    val cpg = code("""
        |struct Widget {
        |  int scale(int factor) const { return factor; }
        |};
        |void use() {
        |  int (Widget::*scaleRef)(int) const = &Widget::scale;
        |}
        |""".stripMargin)

    "give the method a full name carrying the const suffix" in {
      inside(cpg.method.nameExact("scale").l) { case List(scale) =>
        scale.fullName shouldBe s"Widget.scale:int(int)${Defines.ConstSuffix}"
      }
    }

    "give the reference the method's full name" in {
      inside(cpg.methodRef.codeExact("scale").l) { case List(scaleRef) =>
        scaleRef.methodFullName shouldBe s"Widget.scale:int(int)${Defines.ConstSuffix}"
      }
    }

    "have a REF edge from the reference to the method" in {
      cpg.methodRef.codeExact("scale").referencedMethod.fullName.l shouldBe
        List(s"Widget.scale:int(int)${Defines.ConstSuffix}")
    }
  }

  "A reference to one of two overloads differing only in constness" should {

    val cpg = code("""
        |struct Widget {
        |  int scale(int factor) { return factor; }
        |  int scale(int factor) const { return factor; }
        |};
        |void use() {
        |  int (Widget::*mutableRef)(int) = &Widget::scale;
        |  int (Widget::*constRef)(int) const = &Widget::scale;
        |}
        |""".stripMargin)

    "give the two methods distinct full names" in {
      cpg.method.nameExact("scale").fullName.sorted.l shouldBe
        List("Widget.scale:int(int)", s"Widget.scale:int(int)${Defines.ConstSuffix}")
    }

    "name the overload each reference was declared against" in {
      inside(cpg.methodRef.codeExact("scale").sortBy(_.lineNumber).l) { case List(mutableRef, constRef) =>
        mutableRef.methodFullName shouldBe "Widget.scale:int(int)"
        constRef.methodFullName shouldBe s"Widget.scale:int(int)${Defines.ConstSuffix}"
      }
    }

    "resolve each reference to the overload it names" in {
      inside(cpg.methodRef.codeExact("scale").sortBy(_.lineNumber).l) { case List(mutableRef, constRef) =>
        mutableRef.referencedMethod.fullName shouldBe "Widget.scale:int(int)"
        constRef.referencedMethod.fullName shouldBe s"Widget.scale:int(int)${Defines.ConstSuffix}"
      }
    }
  }

  "A reference to a non-const member function" should {

    val cpg = code("""
        |struct Widget {
        |  int bump(int factor) { return factor; }
        |};
        |void use() {
        |  int (Widget::*bumpRef)(int) = &Widget::bump;
        |}
        |""".stripMargin)

    "give the method a full name carrying no const suffix" in {
      inside(cpg.method.nameExact("bump").l) { case List(bump) =>
        bump.fullName shouldBe "Widget.bump:int(int)"
      }
    }

    "give the reference the unsuffixed full name" in {
      inside(cpg.methodRef.codeExact("bump").l) { case List(bumpRef) =>
        bumpRef.methodFullName shouldBe "Widget.bump:int(int)"
      }
    }

    "have a REF edge from the reference to the method" in {
      cpg.methodRef.codeExact("bump").referencedMethod.fullName.l shouldBe List("Widget.bump:int(int)")
    }
  }

  "A reference to a static member function" should {

    val cpg = code("""
        |struct Widget {
        |  static int make(int factor) { return factor; }
        |};
        |void use() {
        |  int (*makeRef)(int) = &Widget::make;
        |}
        |""".stripMargin)

    "give the method a full name carrying no const suffix" in {
      inside(cpg.method.nameExact("make").l) { case List(make) =>
        make.fullName shouldBe "Widget.make:int(int)"
      }
    }

    "give the reference the unsuffixed full name" in {
      inside(cpg.methodRef.codeExact("make").l) { case List(makeRef) =>
        makeRef.methodFullName shouldBe "Widget.make:int(int)"
      }
    }

    "have a REF edge from the reference to the method" in {
      cpg.methodRef.codeExact("make").referencedMethod.fullName.l shouldBe List("Widget.make:int(int)")
    }
  }

  "A reference to a function in a namespace" should {

    val cpg = code("""
        |namespace util {
        |  int clamp(int value) { return value; }
        |}
        |void use() {
        |  int (*clampRef)(int) = &util::clamp;
        |}
        |""".stripMargin)

    "give the method a full name carrying no const suffix" in {
      inside(cpg.method.nameExact("clamp").l) { case List(clamp) =>
        clamp.fullName shouldBe "util.clamp:int(int)"
      }
    }

    "give the unsuffixed full name to the definition's own reference and to the use site alike" in {
      inside(cpg.methodRef.codeExact("clamp").sortBy(_.lineNumber).l) { case List(definitionRef, useRef) =>
        definitionRef.methodFullName shouldBe "util.clamp:int(int)"
        useRef.methodFullName shouldBe "util.clamp:int(int)"
      }
    }
  }

}
