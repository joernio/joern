package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class ConfigFileCreationPass(cpg: Cpg, config: Py2CpgOnFileSystemConfig)
    extends XConfigFileCreationPass(cpg, config = config) {

  override val configFileFilters: List[Path => Boolean] = List(
    // TOML files
    extensionFilter(".toml"),
    // INI files
    extensionFilter(".ini"),
    // YAML files
    extensionFilter(".yaml"),
    // JSON files
    extensionFilter(".json"),
    // Jinja2 templates
    extensionFilter(".jinja2"),
    // HTML files
    extensionFilter(".html"),
    // HTM files
    extensionFilter(".htm"),
    // Requirements.txt
    pathEndFilter("requirements.txt"),
    // Pipfile
    pathEndFilter("Pipfile"),
    pathEndFilter("Pipfile.lock")
  )

}
