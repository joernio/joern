package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class TypeDeclTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      | package Foo;
      | abstract class Bar extends Woo {
      |   int x;
      |   int method () { return 1; }
      | };
      | class Woo {}
      | """.stripMargin).cpg

  "should contain a type decl for `foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Bar").l
    x.name shouldBe "Bar"
    x.code shouldBe "abstract class Bar extends Foo.Woo"
    x.fullName shouldBe "Foo.Bar"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("Foo.Woo")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(".class") shouldBe true
  }

  "should contain type decl for external type `int`" in {
    val List(x) = cpg.typeDecl("int").l
    x.name shouldBe "int"
    x.fullName shouldBe "int"
    x.isExternal shouldBe true
    x.inheritsFromTypeFullName shouldBe List()
    x.aliasTypeFullName shouldBe None
    x.order shouldBe -1
    x.filename shouldBe FileTraversal.UNKNOWN
  }

  "should contain the correct modifier(s)" in {
    val List(x) = cpg.typeDecl.name("Bar").l
    val List(m) = x.modifier.l
    m.modifierType shouldBe ModifierTypes.ABSTRACT
  }

}
