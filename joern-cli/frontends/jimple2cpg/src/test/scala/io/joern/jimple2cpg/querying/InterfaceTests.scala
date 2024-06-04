package io.joern.jimple2cpg.querying
import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language._

import java.io.File

class InterfaceTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
      |interface Foo {
      |
      |   int add(int x, int y);
      |
      |}
      |""".stripMargin).cpg

  "should contain a type decl for `Foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Foo").l
    x.name shouldBe "Foo"
    x.code shouldBe "interface Foo extends java.lang.Object"
    x.fullName shouldBe "Foo"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (startWith(File.separator) or startWith regex "[A-Z]:")
    x.filename.endsWith(".class") shouldBe true
  }

  "should contain a method for `Foo.add` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Foo").method.l
    x.name shouldBe "add"
    x.code shouldBe "int add(int param1, int param2)"
    x.fullName shouldBe "Foo.add:int(int,int)"
    x.isExternal shouldBe false
    x.order shouldBe 1
    x.parameter.code.l shouldBe List("this", "int param1", "int param2")
  }

  "should contain the correct modifier(s)" in {
    val List(x)      = cpg.typeDecl.name("Foo").l
    val List(m1, m2) = x.modifier.l
    m1.modifierType shouldBe ModifierTypes.ABSTRACT
    m2.modifierType shouldBe "INTERFACE"
  }

}
