package io.joern.ghidra2cpg.querying.arm

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.{ICallResolver, _}

class CallNodeTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/arm/aarch64_bin")
  }

  "CPG for binary of a simple program should not contain any CALL nodes with more than two arguments and the same ARGUMENT_INDEX value" in {
    cpg.call
      .filter { c =>
        c.argument.size > 1 && c.argument.argumentIndex.toSet.size == 1
      }
      .code
      .l shouldBe List()
  }

  "An assignment in the function 'main' should have 14 as second argument" in {
    cpg.call
      .name("<operator>.assignment")
      .where(_.method.name("main"))
      .where(
        _.argument
          .order(2)
          .code("14")
      )
      .l match {
      case List(x) =>
        x.name shouldBe "<operator>.assignment"
      case _ => fail()
    }
  }

  "A call to 'test' in 'main' should have 'w0' as argument" in {
    // TODO this technically correct but we are interested in the 0x14
    // mov        w0,#0x14
    // bl         test
    cpg.call
      .name("test")
      .where(_.method.name("main"))
      .where(
        _.argument
          .order(1)
          .code("w0")
      )
      .l match {
      case List(x) =>
        x.name shouldBe "test"
      case _ => fail()
    }
  }

  "The function 'main' should have the following list of calls" in {
    // TODO: resolve locals
    //    eg. #0x10 => #local_10
    cpg.method
      .name("main")
      .call
      .code
      .l
      .mkString(
        " "
      ) shouldBe """stp x29,x30,[sp, #-0x20]! mov x29,sp str w0,[sp, #0x1c] str x1,[sp, #0x10] adrp x0,0x400000 add x0,x0,#0x770 puts mov w0,#0x14 test mov w0,#0x0 ldp x29,x30,[sp], #0x20"""
  }

}
