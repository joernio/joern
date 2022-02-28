package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.Main.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg

object TestAstOnlyFixture {
  def apply(code: String, fileName: String = "file.c")(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / fileName
      file.write(code)
      val cpg = Cpg.emptyCpg
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, None, Config(inputPaths = Set(dir.path.toString)))
        .createAndApply()
      f(cpg)
    }
  }
}

trait TestAstOnlyFixture
