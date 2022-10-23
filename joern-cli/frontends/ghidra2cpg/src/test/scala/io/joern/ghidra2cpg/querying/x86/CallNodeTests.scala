package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.{ICallResolver, _}

class CallNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/x86/64/x86_64.bin")
  }

  "A call should contain exactly one node with all mandatory fields set" in {
    def result = cpg.call
      .name("<operator>.assignment")
      .where(_.method.name("main"))
      .where(
        _.argument
          .order(2)
          .code("0xa")
      )
      .l
    result match {
      case List(x) =>
        x.name shouldBe "<operator>.assignment"
      case _ => fail()
    }
  }

  "A method with name 'main' should have a call with the according code" in {
    val result = cpg.method
      .name("main")
      .call
      .name("<operator>.assignment")
      .where(
        _.argument
          .order(2)
          .code("0xa")
      )
      .head
      .code

    result shouldBe "MOV dword ptr [RBP + -0x4],0xa"
  }

  "A call should have a method with the name 'main' " in {
    val result = cpg.call
      .name("<operator>.assignment")
      .where(_.argument.order(2).code("0xa"))
      .method
      .l
      .last

    result.name shouldBe "main"
  }
  // TODO: should be level2->level1->main
  "The caller of the caller of 'level2' should be 'level1' " in {
    implicit val resolver: ICallResolver = NoResolve

    val result = cpg.method
      .name("level2")
      .caller
      .l
      .head

    result.name shouldBe "level1"
  }

  "The method 'level2' should have a node with the name 'level1' " in {
    implicit val resolver: ICallResolver = NoResolve
    val result = cpg.method
      .name("level2")
      .caller
      .l
      .head

    result.name shouldBe "level1"
  }
}
