package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.parser.FileDefaults
import io.joern.x2cpg.testfixtures.CfgTestCpg

class CCfgTestCpg(override val fileSuffix: String = FileDefaults.CExt) extends CfgTestCpg with C2CpgFrontend {}
