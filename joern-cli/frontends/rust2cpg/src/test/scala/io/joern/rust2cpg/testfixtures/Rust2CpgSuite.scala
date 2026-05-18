package io.joern.rust2cpg.testfixtures

import io.joern.rust2cpg.Config
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class Rust2CpgSuite(withPostProcessing: Boolean = false, noSysRoot: Boolean = false)
    extends Code2CpgFixture(() =>
      RustDefaultTestCpg()
        .withConfig(Config().withNoSysRoot(noSysRoot))
        .withPostProcessingPasses(withPostProcessing)
    ) {

  override def code(code: String): RustDefaultTestCpg = {
    super.code(code, (Paths.get("src") / "lib.rs").toString)
  }

}
