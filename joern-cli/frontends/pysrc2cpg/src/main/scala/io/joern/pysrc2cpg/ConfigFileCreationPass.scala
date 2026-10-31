package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class ConfigFileCreationPass(cpg: Cpg, config: Py2CpgOnFileSystemConfig)
    extends XConfigFileCreationPass(cpg, config = config) {

  override val configFileFilters: List[Path => Boolean] = {
    extensionFilter(".jinja2") ::
      pathEndFilter("requirements.txt") ::
      pathEndFilter("Pipfile") ::
      pathEndFilter("Pipfile.lock") ::
      DefaultFileExtensionFilters
  }

}
