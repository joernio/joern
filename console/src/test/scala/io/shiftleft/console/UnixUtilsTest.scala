package io.shiftleft.console

import better.files._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnixUtilsTest extends AnyWordSpec with Matchers {

  "writes to file (overwrite)" when {
    "using String" in {
      File.usingTemporaryFile() { outFile =>
        outFile.write("existing content in file")

        val content = "some content"
        content |> outFile.pathAsString

        outFile.contentAsString shouldBe content
      }
    }

    "using List" in {
      File.usingTemporaryFile() { outFile =>
        outFile.write("existing content in file")

        val content1 = "some content 1"
        val content2 = "some content 2"
        List(content1, content2) |> outFile.pathAsString

        outFile.contentAsString shouldBe s"$content1\n$content2"
      }
    }
  }

  "appends to file" when {
    "using String" in {
      File.usingTemporaryFile() { outFile =>
        val existingContent = "existing content in file"
        outFile.write(existingContent)

        val content = "some content"
        content |>> outFile.pathAsString

        outFile.contentAsString shouldBe s"$existingContent\n$content"
      }
    }

    "using List" in {
      File.usingTemporaryFile() { outFile =>
        val existingContent = "existing content in file"
        outFile.write(existingContent)

        val content1 = "some content 1"
        val content2 = "some content 2"
        Set(content1, content2) |>> outFile.pathAsString

        outFile.contentAsString shouldBe s"$existingContent\n$content1\n$content2"
      }
    }
  }

}
