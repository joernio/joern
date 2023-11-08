package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class ClassTypeTests extends CCodeToCpgSuite(FileDefaults.CPP_EXT) {

  "handling C++ classes (code example 1)" should {
    val cpg = code("""
      |class Foo {
      |  member_type x;
      |};
      |
      |ret_type myFunc(param_type param) {
      |  local_type y;
      |}""".stripMargin)

    "create TYPE node with correct fields for class member" in {
      val List(x) = cpg.typ.name("member_type").l
      x.fullName shouldBe "member_type"
      x.typeDeclFullName shouldBe "member_type"
    }

    "create TYPE node with correct fields for return type" in {
      val List(x) = cpg.typ.name("ret_type").l
      x.fullName shouldBe "ret_type"
      x.typeDeclFullName shouldBe "ret_type"
    }

    "create TYPE node with correct fields for parameter type" in {
      val List(x) = cpg.typ.name("param_type").l
      x.fullName shouldBe "param_type"
      x.typeDeclFullName shouldBe "param_type"
    }

    "create TYPE node with correct fields for local type" in {
      val List(x) = cpg.typ.name("local_type").l
      x.fullName shouldBe "local_type"
      x.typeDeclFullName shouldBe "local_type"
    }

    "allow traversing from member's TYPE to member" in {
      val List(x) = cpg.typ("member_type").memberOfType.l
      x.name shouldBe "x"
    }

    "allow traversing from return params TYPE to return param" in {
      val List(x) = cpg.typ("ret_type").methodReturnOfType.l
      x.typeFullName shouldBe "ret_type"
    }

    "allow traversing from params TYPE to param" in {
      val List(x) = cpg.typ("param_type").parameterOfType.l
      x.name shouldBe "param"
    }

    "allow traversing from local's TYPE to local" in {
      val List(x) = cpg.typ("local_type").localOfType.l
      x.name shouldBe "y"
    }
  }

  "handling C++ classes (code example 2)" should {
    val cpg = code("""
      |class foo : bar {
      |  char x;
      |  int y;
      |  int method () {}
      |};
      |typedef int mytype;""".stripMargin)

    "should contain a type decl for `foo` with correct fields" in {
      val List(x) = cpg.typeDecl("foo").l
      x.fullName shouldBe "foo"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List("bar")
      x.aliasTypeFullName shouldBe None
      x.order shouldBe 1
      x.filename shouldBe "Test0.cpp"
      x.filename.endsWith(FileDefaults.CPP_EXT) shouldBe true
    }

    "should contain type decl for alias `mytype` of `int`" in {
      val List(x) = cpg.typeDecl("mytype").l
      x.fullName shouldBe "mytype"
      x.isExternal shouldBe false
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe Some("int")
      x.code shouldBe "typedef int mytype;"
      x.order shouldBe 2
      x.filename shouldBe "Test0.cpp"
      x.filename.endsWith(FileDefaults.CPP_EXT) shouldBe true
    }

    "should contain type decl for external type `int`" in {
      val List(x) = cpg.typeDecl("int").l
      x.fullName shouldBe "int"
      x.isExternal shouldBe true
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe None
      x.order shouldBe -1
      x.filename shouldBe "<includes>"
    }

    "should find exactly 1 internal type" in {
      cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).internal.name.toSetMutable shouldBe Set("foo")
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
  "handling C++ classes (code example 3)" should {
    "generate correct call fullnames" in {
      val cpg = code("""
        |class B {
        |public:
        |  void foo2() {}
        |};
        |
        |class A {
        |private:
        |  B b;
        |
        |public:
        |  void foo1() {
        |    b.foo2();
        |   }
        |};
        |
        |int main() {
        |  A a;
        |  a.foo1();
        |  return 0;
        |}""".stripMargin)

      val List(call) = cpg.call("foo2").l
      call.methodFullName shouldBe "B.foo2"
    }
  }

}
