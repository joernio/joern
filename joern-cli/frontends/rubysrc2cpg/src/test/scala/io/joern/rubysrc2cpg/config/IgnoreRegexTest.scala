package io.joern.rubysrc2cpg.config

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class IgnoreRegexTest extends RubyCode2CpgFixture {
  "File matching default ignore regex should be skipped" in {
    val cpg = code(
      """
        |puts "test file"
        |""".stripMargin,
      "tmpdir/db/migrate/test0.rb"
    )

    cpg.file.map(_.name).contains("tmpdir/db/migrate/test0.rb") shouldBe false
  }
}
