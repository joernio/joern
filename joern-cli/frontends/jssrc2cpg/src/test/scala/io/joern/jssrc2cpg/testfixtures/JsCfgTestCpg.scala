package io.joern.jssrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.CfgTestCpg

class JsCfgTestCpg extends CfgTestCpg with JsSrc2CpgFrontend {
  override val fileSuffix: String = ".js"
}
