package io.joern.x2cpg

import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SourceFilesTests extends AnyWordSpec with Matchers {

  val cSourceFileExtensions = Set(".c", ".h")
  val resourcesRoot         = ProjectRoot.relativise("joern-cli/frontends/x2cpg/src/main/resources")

  "determine source files" when {

    "using regular input file" in {
      SourceFiles.determine(s"$resourcesRoot/testcode/main.c", cSourceFileExtensions).size shouldBe 1
    }

    "using regular input directory" in {
      SourceFiles.determine(s"$resourcesRoot/testcode", cSourceFileExtensions).size shouldBe 3
    }

    "input is symlink to file" in {
      SourceFiles.determine(s"$resourcesRoot/symlink-to-main.c", cSourceFileExtensions).size shouldBe 1
    }

    "input is symlink to directory" in {
      SourceFiles.determine(s"$resourcesRoot/symlink-to-testcode", cSourceFileExtensions).size shouldBe 3
    }

  }
}
