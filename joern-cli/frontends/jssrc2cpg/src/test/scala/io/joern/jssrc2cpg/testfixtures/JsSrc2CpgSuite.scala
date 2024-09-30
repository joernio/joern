package io.joern.jssrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class JsSrc2CpgSuite(
  fileSuffix: String = ".js",
  withOssDataflow: Boolean = false,
  semantics: Semantics = DefaultSemantics(),
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new JsSrcDefaultTestCpg(fileSuffix)
        .withOssDataflow(withOssDataflow)
        .withSemantics(semantics)
        .withPostProcessingPasses(withPostProcessing)
    )
