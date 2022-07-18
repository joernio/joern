package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

abstract class AbstractPassTest extends AnyWordSpec with Matchers with Inside {

  protected abstract class Fixture

  protected object AstFixture extends Fixture {
    def apply(code: String, filename: String = "code.js")(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { dir =>
        val cpg  = newEmptyCpg()
        val file = dir / filename
        file.write(code)
        val config       = Config(inputPath = dir.toString(), outputPath = dir.toString())
        val astgenResult = AstGenRunner.execute(config, dir)
        new AstCreationPass(cpg, astgenResult, config).createAndApply()
        f(cpg)
        file.delete()
      }
    }

    def apply(testFile: File)(f: Cpg => Unit): Unit = {
      val file         = testFile
      val dir          = file.parent
      val cpg          = newEmptyCpg()
      val config       = Config(inputPath = dir.toString(), outputPath = dir.toString())
      val astgenResult = AstGenRunner.execute(config, dir)
      new AstCreationPass(cpg, astgenResult, config).createAndApply()
      f(cpg)
    }
  }

  protected object CfgFixture extends Fixture {
    def apply(code: String, filename: String = "code.js")(f: Cpg => Unit): Unit = {
      AstFixture(code, filename) { cpg =>
        new CfgCreationPass(cpg).createAndApply()
        f(cpg)
      }
    }
  }

  protected object TsAstFixture extends Fixture {
    def apply(code: String, filename: String = "code.ts")(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { dir =>
        val cpg  = newEmptyCpg()
        val file = dir / filename
        file.write(code)
        val config          = Config(inputPath = dir.toString(), outputPath = dir.toString())
        val astgenResult    = AstGenRunner.execute(config, dir)
        val astCreationPass = new AstCreationPass(cpg, astgenResult, config)
        astCreationPass.createAndApply()
        new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        f(cpg)
        file.delete()
      }
    }
  }

}
