package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class ImplementsInterfaceTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
        |import java.io.Serializable;
        |
        |final class Foo implements Serializable {
        |
        |   public int add(int x, int y) {
        |     return x + y;
        |   }
        |
        |}
        |""".stripMargin).cpg

  "should contain a type decl for `Foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Foo").l
    x.name shouldBe "Foo"
    x.code shouldBe "final class Foo implements java.io.Serializable"
    x.fullName shouldBe "Foo"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("java.io.Serializable")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (startWith(File.separator) or startWith regex "[A-Z]:")
    x.filename.endsWith(".class") shouldBe true
  }

  "should contain a type decl for `Serializable` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Serializable").l
    x.name shouldBe "Serializable"
    x.fullName shouldBe "java.io.Serializable"
    x.isExternal shouldBe true
    x.aliasTypeFullName shouldBe None
    x.order shouldBe -1
    x.filename shouldBe FileTraversal.UNKNOWN
  }

}
