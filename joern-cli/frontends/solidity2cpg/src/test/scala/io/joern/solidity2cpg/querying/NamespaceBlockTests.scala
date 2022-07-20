package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class NamespaceBlockTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """// since solidity does not have a notation of declaring a "package" keyword
      |// the full namespace must be determined from the file structure from the project root
      |contract A {
      | function foo() public {}
      |}
      |""".stripMargin

  "should contain two namespace blocks in total (<default>, A)" in {
    cpg.namespaceBlock.size shouldBe 2
  }

//  "should contain correct namespace block for known file" in {
//    println(cpg.namespaceBlock.fullName.l)
//    val List(x) = cpg.namespaceBlock.filename(".*.sol").l
//    x.name shouldBe "A"
//    x.filename should not be ""
//    x.fullName shouldBe s"A"
//    x.order shouldBe 1
//  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filename(".*.sol").typeDecl.method.name.toSetMutable shouldBe Set("foo")
  }

  "should allow traversing from namespace block to type declaration" in {
    cpg.namespaceBlock.filename(".*.sol").typeDecl.name.l shouldBe List("A")
  }

//  "should allow traversing from namespace block to namespace" in {
//    cpg.namespaceBlock.filename(".*.sol").namespace.name.l shouldBe List("bar")
//  }

}
