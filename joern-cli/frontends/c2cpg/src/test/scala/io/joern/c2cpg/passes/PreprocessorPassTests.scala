package io.joern.c2cpg.passes

import io.joern.c2cpg.Config
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class PreprocessorPassTests extends AnyWordSpec with Matchers {

  "PreprocessorPass" should {

    "find all ifdefs and ifs" in {
      FileUtil.usingTemporaryDirectory("preprocessorPassTests") { dir =>
        val code =
          """
            |#ifdef SYMBOL
            | foo();
            |#endif
            |
            |#define FOO
            |#ifndef SYMBOL
            | bar();
            |#endif
            |#ifdef FOO
            | main();
            |#endif
            |""".stripMargin

        val filename = "test.c"
        val file     = dir / filename
        Files.writeString(file, code)
        val config = Config().withInputPath(dir.toString)
        val stmts  = new PreprocessorPass(config).run().toList
        stmts shouldBe List("SYMBOL", "FOO=true")
      }
    }

    "find all ifdefs and ifs with predefined symbols" in {
      FileUtil.usingTemporaryDirectory("preprocessorPassTests") { dir =>
        val code =
          """
            |#ifdef SYMBOL
            | foo();
            |#endif
            |
            |#define FOO
            |#ifndef SYMBOL
            | bar();
            |#endif
            |#ifdef FOO
            | main();
            |#endif
            |""".stripMargin

        val filename = "test.c"
        val file     = dir / filename
        Files.writeString(file, code)
        val config = Config(defines = Set("SYMBOL")).withInputPath(dir.toString)
        val stmts  = new PreprocessorPass(config).run().toList
        stmts shouldBe List("SYMBOL=true", "FOO=true")
      }
    }

  }

}
