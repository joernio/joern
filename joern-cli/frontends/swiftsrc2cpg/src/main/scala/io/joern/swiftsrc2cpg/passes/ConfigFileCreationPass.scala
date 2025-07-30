package io.joern.swiftsrc2cpg

import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class ConfigFileCreationPass(cpg: Cpg) extends XConfigFileCreationPass(cpg) {

  override val configFileFilters: List[Path => Boolean] = List(extensionFilter(".plist"), extensionFilter(".xib"))

}
