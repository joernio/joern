package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.Config
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
        val file     = dir / filename
        file.write(code)
        val config = Config(inputPath = dir.path.toString, includePathsAutoDiscovery = false)

        val stmts = new PreprocessorPass(config).run().toList
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
        val file     = dir / filename
        file.write(code)
        val config =
          Config(inputPath = dir.path.toString, defines = Set("SYMBOL"), includePathsAutoDiscovery = false)
        val stmts = new PreprocessorPass(config).run().toList
        stmts shouldBe List("SYMBOL=true", "FOO=true")
      }
    }

  }

}
