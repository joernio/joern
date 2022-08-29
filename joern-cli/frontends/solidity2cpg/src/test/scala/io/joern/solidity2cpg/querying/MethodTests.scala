package io.joern.solidity2cpg.querying
import io.shiftleft.semanticcpg.language._
import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture

import java.io.File

class MethodTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """
      | pragma solidity ^0.8.0;
      | contract Foo {
      |   function foo(int param1, int param2) public returns (uint8) {
      |     return 1;
      |   }
      | }
      |""".stripMargin

  "should contain exactly one non-stub method node with correct fields" in {
    // <init> is reserved for constructors in some frontends
    val List(x) = cpg.method.nameNot("<init>").isExternal(false).l
    x.name shouldBe "foo"
    x.fullName shouldBe "Foo.foo:uint8(int,int)"
    x.code shouldBe "function foo(int param1, int param2) public returns (uint8)"
    x.signature shouldBe "uint8(int,int)"
    x.isExternal shouldBe false
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(".sol") shouldBe true
//    x.lineNumber shouldBe Some(2)
//    x.lineNumberEnd shouldBe Some(4)
//    x.columnNumber shouldBe Some(4)
//    x.columnNumberEnd shouldBe Some(4)
  }

//  "should return correct number of lines" in {
//    cpg.method.name("foo").numberOfLines.l shouldBe List(3)
//  }

  "should allow traversing to parameters" in {
    cpg.method.name("foo").parameter.name.toSetMutable shouldBe Set("this", "param1", "param2")
  }

  "should allow traversing to methodReturn" in {
    cpg.method.name("foo").methodReturn.typeFullName.l shouldBe List("uint8")
  }

  "should allow traversing to file" in {
    cpg.method.name("foo").file.name.l should not be empty
  }

}
