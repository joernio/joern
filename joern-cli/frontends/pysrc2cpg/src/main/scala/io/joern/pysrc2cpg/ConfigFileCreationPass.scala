package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class ConfigFileCreationPass(cpg: Cpg, requirementsTxt: String = "requirement.txt")
    extends XConfigFileCreationPass(cpg) {

  override val configFileFilters: List[Path => Boolean] = List(
    // TOML files
    extensionFilter(".toml"),
    // INI files
    extensionFilter(".ini"),
    // YAML files
    extensionFilter(".yaml"),
    // HTML files
    extensionFilter(".html"),
    // HTM files
    extensionFilter(".htm"),
    // Requirements.txt
    pathEndFilter(requirementsTxt),
    // Pipfile
    pathEndFilter("Pipfile"),
    pathEndFilter("Pipfile.lock")
  )

}
