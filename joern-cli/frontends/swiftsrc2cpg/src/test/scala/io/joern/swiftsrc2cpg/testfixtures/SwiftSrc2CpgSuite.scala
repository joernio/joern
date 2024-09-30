package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class SwiftSrc2CpgSuite(
  fileSuffix: String = ".swift",
  withOssDataflow: Boolean = false,
  semantics: Semantics = DefaultSemantics(),
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new SwiftDefaultTestCpg(fileSuffix)
        .withOssDataflow(withOssDataflow)
        .withSemantics(semantics)
        .withPostProcessingPasses(withPostProcessing)
    )
