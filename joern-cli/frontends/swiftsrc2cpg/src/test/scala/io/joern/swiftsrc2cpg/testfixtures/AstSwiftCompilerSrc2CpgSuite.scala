package io.joern.swiftsrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.Code2CpgFixture

class AstSwiftCompilerSrc2CpgSuite(fileSuffix: String = ".swift")
    extends Code2CpgFixture(() => new SwiftCompilerAstTestCpg(fileSuffix))
