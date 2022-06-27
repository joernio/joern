package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg

object CpgAstOnlyFixture {
  def apply(code: String, fileName: String = "file.c"): Cpg = {
    val cpg = Cpg.emptyCpg
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val config = Config(inputPath = dir.path.toString, includePathsAutoDiscovery = false)
      val file   = dir / fileName
      file.write(code)
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, config)
        .createAndApply()
    }
    cpg
  }
}

trait CpgAstOnlyFixture
