package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class ConfigFileCreationPass(cpg: Cpg, config: Config)
    extends XConfigFileCreationPass(
      cpg,
      config = config.withDefaultIgnoredFilesRegex(AstGenRunner.AstGenDefaultIgnoreRegex)
    ) {

  override val configFileFilters: List[Path => Boolean] = List(extensionFilter(".plist"), extensionFilter(".xib"))

}
