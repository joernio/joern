package io.joern.x2cpg.utils

import better.files.File
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class ExternalCommandTest extends AnyWordSpec with Matchers {

  "ExternalCommand.run" should {
    "be able to run `ls` successfully" in {
      File.usingTemporaryDirectory("sample") { sourceDir =>
        val cmd = Seq("ls", sourceDir.pathAsString)
        ExternalCommand.run(cmd, sourceDir.pathAsString).toTry should be a Symbol("success")
      }
    }
  }

}
