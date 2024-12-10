package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.parser.FileDefaults
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class AstC2CpgSuite(fileSuffix: String = FileDefaults.CExt) extends Code2CpgFixture(() => new CAstTestCpg(fileSuffix))
