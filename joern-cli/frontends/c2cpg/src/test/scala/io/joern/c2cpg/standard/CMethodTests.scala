package io.joern.c2cpg.standard

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language._

import java.io.File

class CMethodTest1 extends CCodeToCpgSuite {

  override val code: String =
    """
      |  int main(int argc, char **argv) {
      | }""".stripMargin

  "should contain exactly one method node with correct fields" in {
    inside(cpg.method.name("main").l) { case List(x) =>
      x.name shouldBe "main"
      x.fullName shouldBe "main"
      x.code shouldBe "int main (int argc,char **argv)"
      x.signature shouldBe "int main (int,char**)"
      x.isExternal shouldBe false
      x.order shouldBe 1
      x.filename should (
        startWith(File.separator) or // Unix
          startWith regex "[A-Z]:"   // Windows
      )
      x.lineNumber shouldBe Some(2)
      x.lineNumberEnd shouldBe Some(3)
      x.columnNumber shouldBe Some(3)
      x.columnNumberEnd shouldBe Some(2)
    }
  }

  "should return correct number of lines" in {
    cpg.method.name("main").numberOfLines.l shouldBe List(2)
  }

  "should allow traversing to parameters" in {
    cpg.method.name("main").parameter.name.toSetMutable shouldBe Set("argc", "argv")
  }

  "should allow traversing to methodReturn" in {
    cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("int")
  }

  "should allow traversing to file" in {
    cpg.method.name("main").file.name.l should not be empty
  }

}

class CMethodTest2 extends CCodeToCpgSuite {
  override val code = "int foo(); int bar() { return woo(); }"

  "should identify method as stub" in {
    cpg.method.isStub.name.toSetMutable shouldBe Set("<global>", "foo", "woo")
    cpg.method.isNotStub.name.l shouldBe List("bar")
  }
}

class CMethodTest3 extends CCodeToCpgSuite {
  override val code = "void doFoo() {}"

  "should not generate a type decl for method definitions" in {
    inside(cpg.method.name("doFoo").l) { case List(x) =>
      x.name shouldBe "doFoo"
      x.fullName shouldBe "doFoo"
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName should endWith("<global>")
    }
    cpg.typeDecl.fullName.l should not contain "doFoo"
  }
}

class CMethodTest4 extends CCodeToCpgSuite {
  override val code = "void doFoo();"

  "should not generate a type decl for method declarations" in {
    inside(cpg.method.name("doFoo").l) { case List(x) =>
      x.name shouldBe "doFoo"
      x.fullName shouldBe "doFoo"
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName should endWith("<global>")
    }
    cpg.typeDecl.fullName.l should not contain "doFoo"
  }
}
