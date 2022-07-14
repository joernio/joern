package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

abstract class AbstractPassTest extends AnyWordSpec with Matchers with Inside {

  protected abstract class Fixture

  protected object AstFixture extends Fixture {
    def apply(code: String, filename: String = "code.js")(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val file = dir / filename
        file.write(code)
        file.deleteOnExit()
        val cpg = new JsSrc2CpgFrontend().execute(dir.toJava)
        f(cpg)
      }
    }

    def apply(testFile: File)(f: Cpg => Unit): Unit = {
      val file = testFile
      val cpg  = new JsSrc2CpgFrontend().execute(file.parent.toJava)
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

}
