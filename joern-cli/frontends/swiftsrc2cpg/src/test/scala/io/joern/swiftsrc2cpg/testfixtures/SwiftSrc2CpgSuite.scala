package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class SwiftSrc2CpgSuite(
  fileSuffix: String = ".swift",
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty,
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new SwiftDefaultTestCpg(fileSuffix)
        .withOssDataflow(withOssDataflow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
