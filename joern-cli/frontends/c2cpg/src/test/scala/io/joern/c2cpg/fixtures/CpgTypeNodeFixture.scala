package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.joern.x2cpg.layers.Base
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}

object CpgTypeNodeFixture {
  def apply(code: String, fileName: String = "test.c")(f: Cpg => Unit): Unit = {
    val cpg = Cpg.emptyCpg
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / fileName
      file.write(code)

      new MetaDataPass(cpg, Languages.NEWC).createAndApply()
      val astCreationPass =
        new AstCreationPass(cpg, AstCreationPass.SourceFiles, Config(inputPaths = Set(dir.path.toString)))
      astCreationPass.createAndApply()
      new TypeNodePass(astCreationPass.usedTypes(), cpg).createAndApply()

      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
    }
    f(cpg)
  }
}

trait CpgTypeNodeFixture
