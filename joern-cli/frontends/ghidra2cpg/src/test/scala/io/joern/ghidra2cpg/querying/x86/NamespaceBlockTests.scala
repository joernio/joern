package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class NamespaceBlockTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("x86_64.bin")
  }

  // The fuzzyC parser currently just ignores namespaces. We place symbols
  // that can't be associated in a file into the namespace "<global>", and
  // those which can in `filename:<global>`

  "should contain two namespace blocks in total" in {
    cpg.namespaceBlock.size shouldBe 2
  }

  "should contain a correct global namespace block for the `<unknown>` file" in {
    val List(x) = cpg.namespaceBlock.filename(FileTraversal.UNKNOWN).l
    x.name shouldBe "<global>"
    x.fullName shouldBe "<global>"
    x.order shouldBe 1
  }

  "should contain correct namespace block for known file" in {
    val List(x) = cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).l
    x.name shouldBe "<global>"
    x.filename should not be ""
    x.fullName shouldBe s"${x.filename}:<global>"
    x.order shouldBe 1
  }

  "should allow traversing from namespace block to method" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).method.name.l shouldBe List(
      "_init",
      "FUN_00101020",
      "__stack_chk_fail",
      "printf",
      "deregister_tm_clones",
      "register_tm_clones",
      "__do_global_dtors_aux",
      "frame_dummy",
      "refNodeTests",
      "dataflow",
      "level3",
      "level2",
      "level1",
      "literalNodeTest",
      "localNodeTests",
      "main",
      "__libc_csu_init",
      "__libc_csu_fini",
      "_fini",
      "_ITM_deregisterTMCloneTable",
      "__stack_chk_fail",
      "printf",
      "__libc_start_main",
      "__gmon_start__",
      "_ITM_registerTMCloneTable",
      "__cxa_finalize"
    )
  }

  // TODO seems type decl for my_struct is not created
//  "should allow traversing from namespace block to type declaration" in {
//    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).typeDecl.name.l shouldBe List("my_struct")
//  }

  "should allow traversing from namespace block to namespace" in {
    cpg.namespaceBlock.filenameNot(FileTraversal.UNKNOWN).namespace.name.l shouldBe List("<global>")
  }

}
