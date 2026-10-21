package io.joern.rust2cpg.passes

import io.joern.rust2cpg.Config
import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

class RustConfigFileCreationPass(cpg: Cpg, config: Config) extends XConfigFileCreationPass(cpg, config = config) {

  override protected val configFileFilters: List[Path => Boolean] =
    List(pathEndFilter("Cargo.toml"), pathEndFilter("Cargo.lock"), pathEndFilter("rust-toolchain.toml"))
}
