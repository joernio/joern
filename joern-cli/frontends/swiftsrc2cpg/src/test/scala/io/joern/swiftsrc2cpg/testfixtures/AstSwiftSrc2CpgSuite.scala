package io.joern.swiftsrc2cpg.testfixtures

import io.joern.x2cpg.testfixtures.Code2CpgFixture

class AstSwiftSrc2CpgSuite(fileSuffix: String = ".swift")
    extends Code2CpgFixture(() => new SwiftDefaultTestCpg(fileSuffix))
