package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.joern.x2cpg.layers.{Base, TypeRelations}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}

object CompleteCpgFixture {
  def apply(code: String, fileName: String = "test.cpp")(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / fileName
      file.write(code)

      val cpg    = Cpg.emptyCpg
      val config = Config(inputPath = dir.path.toString, includePathsAutoDiscovery = false)

      new MetaDataPass(cpg, Languages.NEWC, config.inputPath).createAndApply()
      val astCreationPass =
        new AstCreationPass(cpg, AstCreationPass.SourceFiles, config)
      astCreationPass.createAndApply()
      new CfgCreationPass(cpg).createAndApply()
      new TypeNodePass(astCreationPass.usedTypes(), cpg).createAndApply()
      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
      new TypeRelations().run(context)
      f(cpg)
    }
  }
}

trait CompleteCpgFixture
