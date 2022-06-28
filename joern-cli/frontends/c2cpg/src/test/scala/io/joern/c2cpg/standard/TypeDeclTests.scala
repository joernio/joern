package io.joern.c2cpg.standard

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class TypeDeclTests extends CCodeToCpgSuite(FileDefaults.CPP_EXT) {

  private val cpg = code("""
   | class foo : bar {
   |   char x;
   |   int y;
   |   int method () {}
   | };
   |
   | typedef int mytype;""".stripMargin)

  "should contain a type decl for `foo` with correct fields" in {
    val List(x) = cpg.typeDecl("foo").l
    x.name shouldBe "foo"
    x.fullName shouldBe "foo"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("bar")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(FileDefaults.CPP_EXT) shouldBe true
  }

  "should contain type decl for alias `mytype` of `int`" in {
    val List(x) = cpg.typeDecl("mytype").l
    x.name shouldBe "mytype"
    x.fullName shouldBe "mytype"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List()
    x.aliasTypeFullName shouldBe Some("int")
    x.code shouldBe "typedef int mytype;"
    x.order shouldBe 2
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(FileDefaults.CPP_EXT) shouldBe true
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
    cpg.typeDecl.nameNot("<global>").internal.name.toSetMutable shouldBe Set("foo")
  }

  "should find five external types (`bar`, `char`, `int`, `void`, `ANY`)" in {
    cpg.typeDecl.external.name.toSetMutable shouldBe Set("bar", "char", "int", "void", "ANY")
  }

  "should find two members for `foo`: `x` and `y`" in {
    cpg.typeDecl.name("foo").member.name.toSetMutable shouldBe Set("x", "y")
  }

  "should allow traversing from `int` to its alias `mytype`" in {
    cpg.typeDecl("int").aliasTypeDecl.name.l shouldBe List("mytype")
    cpg.typeDecl("mytype").aliasTypeDecl.l shouldBe List()
  }

  "should find one method in type `foo`" in {
    cpg.typeDecl.name("foo").method.name.toSetMutable shouldBe Set("method")
  }

  "should allow traversing from type to enclosing file" in {
    cpg.typeDecl.file.filter(_.name.endsWith(FileDefaults.CPP_EXT)).l should not be empty
  }

}
