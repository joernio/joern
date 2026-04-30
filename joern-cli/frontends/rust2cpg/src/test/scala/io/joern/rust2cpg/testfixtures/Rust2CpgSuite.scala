package io.joern.rust2cpg.testfixtures

import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.semanticcpg.utils.FileUtil.*
import java.nio.file.Paths

class Rust2CpgSuite(withPostProcessing: Boolean = false)
    extends Code2CpgFixture(() => RustDefaultTestCpg().withPostProcessingPasses(withPostProcessing)) {

  override def code(code: String): RustDefaultTestCpg = {
    super.code(code, (Paths.get("src") / "lib.rs").toString)
  }

}
