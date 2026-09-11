package io.joern.c2cpg.parser

import io.joern.c2cpg.Config
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class HeaderFileFinderTests extends AnyWordSpec with Matchers {

  "HeaderFileFinder" should {

    "return None for a basename not present in the scanned tree" in {
      FileUtil.usingTemporaryDirectory("headerFileFinder") { dir =>
        Files.writeString(dir / "foo.h", "#pragma once")
        val finder = new HeaderFileFinder(Config().withInputPath(dir.toString))
        finder.find("/some/unresolved/path/bar.h") shouldBe None
      }
    }

    "return the only candidate for a unique basename" in {
      FileUtil.usingTemporaryDirectory("headerFileFinder") { dir =>
        val header = dir / "foo.h"
        Files.writeString(header, "#pragma once")
        val finder = new HeaderFileFinder(Config().withInputPath(dir.toString))
        finder.find("/some/unresolved/path/foo.h") shouldBe Some(header.toString)
      }
    }

    "select the closest match among multiple same-named headers" in {
      FileUtil.usingTemporaryDirectory("headerFileFinder") { dir =>
        val configDirA = dir / "include" / "platform" / "family_0001" / "config"
        val configDirB = dir / "include" / "platform" / "family_0002" / "config"
        Files.createDirectories(configDirA)
        Files.createDirectories(configDirB)
        val headerA = configDirA / "shared_config.h"
        val headerB = configDirB / "shared_config.h"
        Files.writeString(headerA, "#pragma once")
        Files.writeString(headerB, "#pragma once")

        val finder = new HeaderFileFinder(Config().withInputPath(dir.toString))
        // exact path matches resolve to themselves
        finder.find(headerA.toString) shouldBe Some(headerA.toString)
        finder.find(headerB.toString) shouldBe Some(headerB.toString)
        // an unresolved path resolves to the candidate with the smallest edit distance
        val unresolved = dir / "include" / "platform" / "family_0002" / "shared_config.h"
        finder.find(unresolved.toString) shouldBe Some(headerB.toString)
      }
    }
  }

}
