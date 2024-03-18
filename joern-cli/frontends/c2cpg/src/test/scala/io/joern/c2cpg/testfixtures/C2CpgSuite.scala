package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.parser.FileDefaults
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class C2CpgSuite(
  fileSuffix: String = FileDefaults.C_EXT,
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty,
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new CDefaultTestCpg(fileSuffix)
        .withOssDataflow(withOssDataflow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
