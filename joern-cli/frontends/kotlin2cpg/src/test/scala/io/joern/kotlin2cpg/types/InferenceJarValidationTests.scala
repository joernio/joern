package io.joern.kotlin2cpg.types

import better.files.File
import io.shiftleft.utils.ProjectRoot
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InferenceJarValidationTests extends AnyFreeSpec with Matchers {
  "Inference jars inside resources " - {
    val inferenceJarDir = File(
      ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/main/resources/inferencejars/")
    )
    "should not contain any jars which are empty files" in {
      inferenceJarDir.list
        .filter(_.hasExtension)
        .filter(_.pathAsString.endsWith("jar"))
        .filter { f =>
          f.size == 0
        }
        .map(_.pathAsString)
        .toSeq shouldBe Seq()
    }
  }
}
