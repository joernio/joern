package io.joern.ghidra2cpg.querying.x86

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class FileTests extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("x86_64.bin")
  }

  "should contain two file nodes in total, both with order=0" in {
    cpg.file.order.l shouldBe List(0, 0)
    cpg.file.name(FileTraversal.UNKNOWN).size shouldBe 1
    cpg.file.nameNot(FileTraversal.UNKNOWN).size shouldBe 1
  }

  "should contain exactly one placeholder file node with `name=\"<unknown>\"/order=0`" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).order.l shouldBe List(0)
    cpg.file(FileTraversal.UNKNOWN).hash.l shouldBe List()
  }

  "should contain exactly one non-placeholder file with absolute path in `name`" in {
    val List(x) = cpg.file.nameNot(FileTraversal.UNKNOWN).l
    x.name should startWith("/")
    // Ghidra-frontend currently does not set hash but should do so
    // in the future
    x.hash shouldBe None
  }

  "should allow traversing from file to its namespace blocks" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSet shouldBe Set("<global>")
  }

  "should allow traversing from file to its methods via namespace block" in {
    cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSet shouldBe Set(
      "__cxa_finalize",
      "_fini",
      "main",
      "level1",
      "deregister_tm_clones",
      "_start",
      "__gmon_start__",
      "level2",
      "__libc_start_main",
      "FUN_00101020",
      "_ITM_deregisterTMCloneTable",
      "level3",
      "_init",
      "literalNodeTest",
      "__do_global_dtors_aux",
      "__stack_chk_fail",
      "register_tm_clones",
      "frame_dummy",
      "_ITM_registerTMCloneTable",
      "printf",
      "dataflow",
      "refNodeTests",
      "localNodeTests",
      "__libc_csu_init",
      "__libc_csu_fini"
    )
  }

  // TODO type decl handling needs to work first
//  "should allow traversing from file to its type declarations via namespace block" in {
//    cpg.file.nameNot(FileTraversal.UNKNOWN).typeDecl.name.toSet shouldBe Set("my_struct")
//  }

  "should allow traversing to namespaces" in {
    cpg.file.namespace.name("<global>").l.size shouldBe 2
  }
}
