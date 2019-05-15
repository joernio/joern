package io.shiftleft.joern

import org.scalatest.{Matchers, WordSpec}

class TypeDeclTests extends WordSpec with Matchers {

  new TestCpg(
    """
      | class foo {
      |   char x;
      |   int y;
      |   int method () {}
      | };
      |
    """.stripMargin
  ) {
    "should find types `foo`, `char`, `int`, and `void`" in {
      cpg.typeDecl.name.toSet shouldBe Set("foo", "char", "int", "void")
    }

    "should find only one internal type (`foo`)" in {
      cpg.typeDecl.internal.name.toSet shouldBe Set("foo")
    }

    "should find three external types (`char`, `int`, `void`)" in {
      cpg.typeDecl.external.name.toSet shouldBe Set("char", "int", "void")
    }

    "should find two members for `foo`: `x` and `y`" in {
      cpg.typeDecl.name("foo").member.name.toSet shouldBe Set("x", "y")
    }

    "should find one method in type `foo`" in {
      cpg.typeDecl.name("foo").method.name.toSet shouldBe Set("method")
    }

    "should allow traversing from type to enclosing file" in {
      cpg.typeDecl.file.filterOnEnd(_.name.endsWith(".c")).l should not be empty
    }
  }
}
