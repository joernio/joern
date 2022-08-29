package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class TypeDeclTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """ // since solidity does not have a notation of declaring a "package" keyword
      | // the full namespace must be determined from the file structure from the project root
      |
      |// Defining child contract
      |contract Bar is Woo {
      | function getValue() external view returns (uint) {
      |   return sum;
      | }
      |}
      |
      |// Defining parent contract
      |contract Woo {
      | uint internal sum;
      | function setValue() external {
      |   uint a = 10;
      |   uint b = 20;
      |   sum = a + b;
      | }
      |}
      | """.stripMargin

  "should contain a type decl for `Bar` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Bar").l
    x.name shouldBe "Bar"
    x.code shouldBe "Bar"
    x.fullName shouldBe "Bar" // since we can't simulate directory structure it'll just be this
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("Woo")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(".sol") shouldBe true
  }

  "should contain a type decl for `Woo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Woo").l
    x.name shouldBe "Woo"
    x.code shouldBe "Woo"
    x.fullName shouldBe "Woo" // since we can't simulate directory structure it'll just be this
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List()
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 2
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(".sol") shouldBe true
  }

  "should contain type decl for external type `uint`" in {

    val List(x) = cpg.member("sum").l
    x.name shouldBe "sum"
    x.typeFullName shouldBe "uint"
    x.code shouldBe "uint internal sum"
  }

}
