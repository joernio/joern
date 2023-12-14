package io.joern.swiftsrc2cpg.passes.ast

import better.files.File
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.passes.AstCreationPass
import io.joern.swiftsrc2cpg.passes.SwiftTypeNodePass
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

abstract class AbstractPassTest extends AnyWordSpec with Matchers with Inside {

  protected abstract class Fixture

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  protected object AstFixture extends Fixture {
    def apply(code: String, filename: String = "code.swift")(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("swiftsrc2cpgTests") { dir =>
        val cpg  = newEmptyCpg()
        val file = dir / filename
        file.write(code)
        val config          = Config().withInputPath(dir.toString).withOutputPath(dir.toString)
        val astGenResult    = new AstGenRunner(config).execute(dir)
        val astCreationPass = new AstCreationPass(cpg, astGenResult, config)
        astCreationPass.createAndApply()
        SwiftTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
        f(cpg)
        file.delete()
      }
    }
  }
}
