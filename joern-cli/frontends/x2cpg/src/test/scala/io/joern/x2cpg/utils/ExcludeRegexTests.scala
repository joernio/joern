package io.joern.x2cpg.utils

import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import java.util.regex.Pattern

class ExcludeRegexTests extends AnyWordSpec with Matchers {

  /** Relative paths use the separator of the operating system, so regexes must accept both `/` and `\`. */
  private val sep = "[/\\\\]"

  private def withProject[T](f: Path => T): T = {
    FileUtil.usingTemporaryDirectory("excludeRegexTests") { dir =>
      Files.createDirectories(dir / "sub" / "deep")
      Seq("a.js", "sub/b.js", "sub/deep/c.js").foreach(file => Files.writeString(dir / file, ""))
      f(dir)
    }
  }

  private def excluded(regex: String, file: String): Boolean = withProject { dir =>
    ExcludeRegex.matchesRelative(regex.r, (dir / file).toString, dir.toString)
  }

  "matchesRelative" should {
    "match the whole relative path" in {
      excluded("a\\.js", "a.js") shouldBe true
      excluded(s"sub${sep}b\\.js", "sub/b.js") shouldBe true
      excluded(s"sub$sep.*", "sub/deep/c.js") shouldBe true
    }

    "not match only a part of the relative path" in {
      excluded("a", "a.js") shouldBe false
      excluded("\\.js", "a.js") shouldBe false
      excluded(s"ub${sep}b\\.js", "sub/b.js") shouldBe false
      excluded("sub", "sub/b.js") shouldBe false
    }

    "match directories" in {
      excluded("sub", "sub") shouldBe true
      excluded(s"sub${sep}deep", "sub/deep") shouldBe true
    }

    "be case-sensitive" in {
      excluded("A\\.JS", "a.js") shouldBe false
    }

    "support regexes starting with .*" in {
      excluded(".*\\.js", "sub/b.js") shouldBe true
      excluded(s".*${sep}deep$sep.*", "sub/deep/c.js") shouldBe true
    }

    "support lookaheads" in {
      val keepDeep = s"(?!sub${sep}deep$sep.*$$).*\\.js"
      excluded(keepDeep, "a.js") shouldBe true
      excluded(keepDeep, "sub/b.js") shouldBe true
      excluded(keepDeep, "sub/deep/c.js") shouldBe false
    }

    "not match anything for the empty regex" in {
      excluded("", "a.js") shouldBe false
    }

    "not match the absolute path" in {
      withProject { dir =>
        val regex = Pattern.quote(dir.toString) + ".*"
        ExcludeRegex.matchesRelative(regex.r, (dir / "a.js").toString, dir.toString) shouldBe false
      }
    }
  }

  "matchesLegacyAbsolute" should {
    "match the whole absolute path" in {
      withProject { dir =>
        val file = (dir / "sub" / "b.js").toString
        ExcludeRegex.matchesLegacyAbsolute(
          (Pattern.quote(dir.toString) + sep + "sub" + sep + ".*").r,
          file
        ) shouldBe true
        ExcludeRegex.matchesLegacyAbsolute(".*b\\.js".r, file) shouldBe true
        ExcludeRegex.matchesLegacyAbsolute("b\\.js".r, file) shouldBe false
        ExcludeRegex.matchesLegacyAbsolute(s"sub${sep}b\\.js".r, file) shouldBe false
      }
    }

    "not match anything for the empty regex" in {
      withProject { dir => ExcludeRegex.matchesLegacyAbsolute("".r, (dir / "a.js").toString) shouldBe false }
    }

    "never match regexes with a negative lookaround, as they would exclude everything" in {
      withProject { dir =>
        val file = (dir / "sub" / "deep" / "c.js").toString
        ExcludeRegex.matchesLegacyAbsolute(s"(?!sub${sep}deep$sep.*$$).*\\.js".r, file) shouldBe false
        ExcludeRegex.matchesLegacyAbsolute(".*(?<!c)\\.js".r, file) shouldBe false
      }
    }
  }

}
