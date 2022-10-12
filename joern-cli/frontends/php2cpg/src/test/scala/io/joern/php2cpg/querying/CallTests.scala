package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class CallTests extends PhpCode2CpgFixture {
  "halt_compiler calls should be created correctly" in {
    val cpg = code("<?php\n__halt_compiler();")

    inside(cpg.call.l) { case List(haltCompiler) =>
      haltCompiler.name shouldBe NameConstants.HaltCompiler
      haltCompiler.methodFullName shouldBe NameConstants.HaltCompiler
      haltCompiler.code shouldBe s"${NameConstants.HaltCompiler}()"
      haltCompiler.lineNumber shouldBe Some(2)
      haltCompiler.astChildren.size shouldBe 0
    }
  }
}
