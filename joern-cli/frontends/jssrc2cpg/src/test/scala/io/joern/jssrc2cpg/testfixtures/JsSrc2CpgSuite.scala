package io.joern.jssrc2cpg.testfixtures

import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class JsSrc2CpgSuite(
  fileSuffix: String = ".js",
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty,
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new JsSrcDefaultTestCpg(fileSuffix)
        .withOssDataflow(withOssDataflow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
