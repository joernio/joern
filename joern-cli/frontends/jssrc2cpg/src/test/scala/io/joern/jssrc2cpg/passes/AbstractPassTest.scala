package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

abstract class AbstractPassTest extends AnyWordSpec with Matchers with Inside {

  protected abstract class Fixture

  protected object AstFixture extends Fixture {
    def apply(code: String, filename: String = "code.js", tsTypes: Boolean = false)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { dir =>
        val cpg  = newEmptyCpg()
        val file = dir / filename
        file.write(code)
        val config       = Config(tsTypes = tsTypes).withInputPath(dir.toString).withOutputPath(dir.toString)
        val astGenResult = new AstGenRunner(config).execute(dir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        f(cpg)
        file.delete()
      }
    }
  }

  protected object TsAstFixture extends Fixture {
    def apply(code: String, filename: String = "code.ts", tsTypes: Boolean = false)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { dir =>
        val cpg  = newEmptyCpg()
        val file = dir / filename
        file.write(code)
        val config          = Config(tsTypes = tsTypes).withInputPath(dir.toString).withOutputPath(dir.toString)
        val astGenResult    = new AstGenRunner(config).execute(dir)
        val astCreationPass = new AstCreationPass(cpg, astGenResult, config)
        astCreationPass.createAndApply()
        new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        f(cpg)
        file.delete()
      }
    }

    def files(code1: String, filename1: String, code2: String, filename2: String, tsTypes: Boolean = false)(
      f: Cpg => Unit
    ): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { dir =>
        val cpg   = newEmptyCpg()
        val file1 = dir / filename1
        file1.write(code1)
        val file2 = dir / filename2
        file2.write(code2)
        val config          = Config(tsTypes = tsTypes).withInputPath(dir.toString).withOutputPath(dir.toString)
        val astGenResult    = new AstGenRunner(config).execute(dir)
        val astCreationPass = new AstCreationPass(cpg, astGenResult, config)
        astCreationPass.createAndApply()
        new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        f(cpg)
        file1.delete()
        file2.delete()
      }
    }
  }

}
