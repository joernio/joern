package io.joern.swiftsrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.CfgTestCpg

class SwiftCfgTestCpg extends CfgTestCpg with SwiftSrc2CpgFrontend {
  override val fileSuffix: String = ".swift"
}
