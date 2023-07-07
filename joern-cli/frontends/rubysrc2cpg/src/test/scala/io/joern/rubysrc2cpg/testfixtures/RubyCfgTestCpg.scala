package io.joern.rubysrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.CfgTestCpg

class RubyCfgTestCpg extends CfgTestCpg with RubyFrontend {
  override val fileSuffix: String = ".rb"

}
