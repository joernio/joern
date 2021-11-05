package io.shiftleft.fuzzyc2cpg.standard

import io.shiftleft.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class TypeDeclTests extends FuzzyCCodeToCpgSuite {

  override val code = """
                   | class foo : bar {
                   |   char x;
                   |   int y;
                   |   int method () {}
                   | };
                   |
                   | typedef int mytype;
                   |
    """.stripMargin

  "should contain a type decl for `foo` with correct fields" in {
    val List(x) = cpg.typeDecl("foo").l
    x.name shouldBe "foo"
    x.fullName shouldBe "foo"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("bar")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename.startsWith("/") shouldBe true
    x.filename.endsWith(".c") shouldBe true
  }

  "should contain type decl for alias `mytype` of `int`" in {
    val List(x) = cpg.typeDecl("mytype").l
    x.name shouldBe "mytype"
    x.fullName shouldBe "mytype"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List()
    x.aliasTypeFullName shouldBe Some("int")
    x.order shouldBe 2
    x.filename.startsWith("/") shouldBe true
    x.filename.endsWith(".c") shouldBe true
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

  "should find exactly 1 internal type" in {
    cpg.typeDecl.internal.name.toSet shouldBe Set("foo")
  }

  "should find five external types (`bar`, `char`, `int`, `void`, `ANY`)" in {
    cpg.typeDecl.external.name.toSet shouldBe Set("bar", "char", "int", "void", "ANY")
  }

  "should find two members for `foo`: `x` and `y`" in {
    cpg.typeDecl.name("foo").member.name.toSet shouldBe Set("x", "y")
  }

  "should allow traversing from `int` to its alias `mytype`" in {
    cpg.typeDecl("int").aliasTypeDecl.name.l shouldBe List("mytype")
    cpg.typeDecl("mytype").aliasTypeDecl.l shouldBe List()
  }

  "should find one method in type `foo`" in {
    cpg.typeDecl.name("foo").method.name.toSet shouldBe Set("method")
  }

  "should allow traversing from type to enclosing file" in {
    cpg.typeDecl.file.filter(_.name.endsWith(".c")).l should not be empty
  }

}
