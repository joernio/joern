package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg

import scala.concurrent.ExecutionContext

object CpgAstOnlyFixture {
  def apply(code: String, fileName: String = "file.c"): Cpg = {
    val cpg = Cpg.emptyCpg
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / fileName
      file.write(code)
      implicit val ec: ExecutionContext = ExecutionContext.global
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, None, Config(inputPaths = Set(dir.path.toString)))
        .createAndApply()
    }
    cpg
  }
}

trait CpgAstOnlyFixture
