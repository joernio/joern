package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.php2cpg.utils.PathFilter
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.semanticcpg.language._
import io.joern.php2cpg.Config

class FileTests extends PhpCode2CpgFixture {
  val testFiles = List(
    "a.php",
    "src/b.php",
    "src/sub/c.php",
    "tests/d.php",
    "tests/sub/e.php",
    "vendor/autoload.php",
    "vendor/dep/f.php",
    "misc/g.php",
    ".git/h.php",
    "sub/tests/i.php",
    "nottests/j.php"
  )

  private def getCpg(): TestCpg = {
    val cpg = code("", fileName = testFiles.head)
    testFiles.tail.foreach(path => cpg.moreCode("", fileName = PathFilter.correctPath(path)))
    cpg
  }

  "test, vendor, and .git directories should be filtered out by default" in {
    val cpg = getCpg()

    cpg.file.name.toSet shouldBe Set("a.php", "src/b.php", "src/sub/c.php", "misc/g.php", "nottests/j.php", "<unknown>")
      .map(PathFilter.correctPath)
  }

  "custom excludes should exclude the correct directories" in {
    val config = Config(excludeOverrides = Some(List("dep", "src/sub/", "doesntexist")))
    val cpg    = getCpg().config(config)

    cpg.file.name.toSet shouldBe Set(
      "a.php",
      "src/b.php",
      "tests/d.php",
      "tests/sub/e.php",
      "vendor/autoload.php",
      "misc/g.php",
      ".git/h.php",
      "sub/tests/i.php",
      "nottests/j.php",
      "<unknown>"
    )
  }
}
