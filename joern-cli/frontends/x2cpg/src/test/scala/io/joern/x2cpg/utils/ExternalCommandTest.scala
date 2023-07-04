package io.joern.x2cpg.utils

import better.files.File
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class ExternalCommandTest extends AnyWordSpec with Matchers {
  "ExternalCommand.run" should {
    "be able to run `ls` successfully" in {
      File.usingTemporaryDirectory("sample") { sourceDir =>
        val cmd = "ls " + sourceDir.pathAsString
        ExternalCommand.run(cmd, sourceDir.pathAsString) should be a Symbol("success")
      }
    }
  }

  "ExternalCommand.runMultiple" should {
    "be able to run multiple `ls` invocations successfully" in {
      File.usingTemporaryDirectory("sample") { sourceDir =>
        File.usingTemporaryFile(parent = Some(sourceDir)) { _ =>
          val cmd    = "ls " + sourceDir.pathAsString + " && " + "ls " + sourceDir.pathAsString
          val result = ExternalCommand.runMultiple(cmd, sourceDir.pathAsString)
          result match {
            case Success(value) => value.split("\n").length shouldBe 2
            case Failure(_)     => false shouldBe true
          }
        }
      }
    }
  }
}
