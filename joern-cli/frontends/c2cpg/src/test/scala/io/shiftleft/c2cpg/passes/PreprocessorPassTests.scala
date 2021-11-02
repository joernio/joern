package io.shiftleft.c2cpg.passes

import better.files.File
import io.shiftleft.c2cpg.parser.ParserConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PreprocessorPassTests extends AnyWordSpec with Matchers {

  "PreprocessorPass" should {

    "find all ifdefs and ifs" in {
      File.usingTemporaryDirectory("preprocessorPassTests") { dir =>
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
        val file = dir / filename
        file.write(code)
        val filePath = file.path.toAbsolutePath.toString

        val stmts = new PreprocessorPass(List(filePath), ParserConfig.empty).run().toList
        stmts shouldBe List("SYMBOL", "FOO=true")
      }
    }

    "find all ifdefs and ifs with predefined symbols" in {
      File.usingTemporaryDirectory("preprocessorPassTests") { dir =>
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
        val file = dir / filename
        file.write(code)
        val filePath = file.path.toAbsolutePath.toString

        val parseConfig = ParserConfig(Set.empty, Map("SYMBOL" -> "true"), logProblems = false, logPreprocessor = false)
        val stmts = new PreprocessorPass(List(filePath), parseConfig).run().toList
        stmts shouldBe List("SYMBOL=true", "FOO=true")
      }
    }

  }

}
