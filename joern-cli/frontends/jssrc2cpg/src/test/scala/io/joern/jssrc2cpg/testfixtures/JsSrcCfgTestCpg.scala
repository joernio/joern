package io.joern.jssrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.CfgTestCpg

class JsSrcCfgTestCpg(override val fileSuffix: String = ".js") extends CfgTestCpg with JsSrc2CpgFrontend {}
