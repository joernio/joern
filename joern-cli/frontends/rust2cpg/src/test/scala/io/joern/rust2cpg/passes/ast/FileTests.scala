package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class FileTests extends Rust2CpgSuite(noSysRoot = true) {

  "a single source file produces one FILE alongside `<unknown>`" in {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg     = code("", libPath)
    cpg.file.name.sorted.l shouldBe List("<unknown>", libPath)
  }

  "each source file produces its own FILE node" in {
    val libPath  = (Paths.get("src") / "lib.rs").toString
    val mainPath = (Paths.get("src") / "main.rs").toString
    val cpg      = code("", libPath).moreCode("", mainPath)
    cpg.file.name.sorted.l shouldBe List("<unknown>", libPath, mainPath)
  }

}
