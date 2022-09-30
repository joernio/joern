package io.joern.console.scripting

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// needed in the latest cast-effect version to run .unsafeRunSync()
import cats.effect.unsafe.implicits.global

import java.nio.file.{Path, Paths}

class AmmoniteExecutorTest extends AnyWordSpec with Matchers {
  private object TestAmmoniteExecutor extends AmmoniteExecutor {
    override protected def predef: String =
      """
        |import io.shiftleft.semanticcpg.language._
        |""".stripMargin
  }

  private def getScriptPath(script: String): Path = {
    val scriptURI = getClass.getClassLoader.getResource(script).toURI
    Paths.get(scriptURI)
  }

  private def withExecutor[T](f: AmmoniteExecutor => T): T = {
    f(TestAmmoniteExecutor)
  }

  "An AmmoniteExecutor" should {
    "execute a single script with an implicit cpg in scope" in withExecutor { executor =>
      val script = getScriptPath("scripts/general/list-funcs.sc")

      executor.runScript(script, Map.empty, Cpg.emptyCpg).unsafeRunSync() shouldBe List()
    }

    "pass arguments to a script" in withExecutor { executor =>
      val script = getScriptPath("scripts/general/arguments-concatenate.sc")

      executor
        .runScript(script, Map("one" -> "hello", "two" -> "world"), Cpg.emptyCpg)
        .unsafeRunSync() shouldBe "hello world"
    }

    "execute multiple scripts" in withExecutor { executor =>
      val script       = getScriptPath("scripts/general/list-funcs.sc")
      val secondScript = getScriptPath("scripts/java/list-sl-ns.sc")

      executor.runScripts(List(script, secondScript), Map.empty, Cpg.emptyCpg).unsafeRunSync() shouldBe
        List(List(), List())
    }

    "return a failure if the script can not be found" in withExecutor { executor =>
      val script = (File(os.pwd.toNIO) / "cake.sc").path

      val ex = intercept[RuntimeException] {
        executor.runScript(script, Map.empty, Cpg.emptyCpg).unsafeRunSync()
      }

      ex.getMessage shouldBe s"Script file not found: ${script.toString}"
    }

    "propagate any exceptions thrown by a script" in withExecutor { executor =>
      val script = getScriptPath("scripts/general/divide_by_zero.sc")

      intercept[ArithmeticException] {
        executor.runScript(script, Map.empty, Cpg.emptyCpg).unsafeRunSync()
      }
    }

    "run a string query" in withExecutor { executor =>
      val query = "cpg.method.l"

      executor.runQuery(query, Cpg.emptyCpg).unsafeRunSync() shouldBe List()
    }

    "propagate errors if the string query fails" in withExecutor { executor =>
      val query = "cake"

      intercept[RuntimeException] {
        executor.runQuery(query, Cpg.emptyCpg).unsafeRunSync()
      }
    }
  }
}
