package io.joern.c2cpg.fixtures

import better.files.File
import io.joern.c2cpg.Main.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.layers.{Base, LayerCreatorContext, TypeRelations}
import io.shiftleft.semanticcpg.passes.controlflow.CfgCreationPass
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}

object CompleteCpgFixture {
  def apply(code: String, fileName: String = "test.cpp")(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / fileName
      file.write(code)

      val cpg = Cpg.emptyCpg
      new MetaDataPass(cpg, Languages.NEWC).createAndApply()
      val astCreationPass =
        new AstCreationPass(cpg, AstCreationPass.SourceFiles, None, Config(inputPaths = Set(dir.path.toString)))
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
