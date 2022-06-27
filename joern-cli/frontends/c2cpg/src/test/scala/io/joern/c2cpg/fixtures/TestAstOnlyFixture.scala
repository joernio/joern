package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg

object TestAstOnlyFixture {
  def apply(code: String, fileName: String = "file.c")(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / fileName
      file.write(code)
      val cpg    = Cpg.emptyCpg
      val config = Config(inputPath = dir.path.toString, includePathsAutoDiscovery = false)
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, config)
        .createAndApply()
      f(cpg)
    }
  }
}

trait TestAstOnlyFixture
