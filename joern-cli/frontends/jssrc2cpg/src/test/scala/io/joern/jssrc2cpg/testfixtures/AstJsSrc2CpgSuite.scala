package io.joern.jssrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.Code2CpgFixture

class AstJsSrc2CpgSuite(fileSuffix: String = ".js") extends Code2CpgFixture(() => new JsSrcAstTestCpg(fileSuffix))
