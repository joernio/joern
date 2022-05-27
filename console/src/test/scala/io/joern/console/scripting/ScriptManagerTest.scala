package io.joern.console.scripting

import cats.effect.IO
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.TestProtoCpg
import io.joern.console.scripting.ScriptManager.{ScriptCollections, ScriptDescription, ScriptDescriptions}
import io.shiftleft.utils.IOUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterAll, Inside}

import java.nio.file.{NoSuchFileException, Path}
import scala.util.Try

class ScriptManagerTest extends AnyWordSpec with Matchers with Inside with BeforeAndAfterAll {

  var zipFile: better.files.File         = _
  protected var DEFAULT_CPG_NAME: String = _

  override def beforeAll(): Unit = {
    zipFile = TestProtoCpg.createTestProtoCpg
    DEFAULT_CPG_NAME = zipFile.pathAsString
  }

  override def afterAll(): Unit = {
    zipFile.delete()
  }

  private object TestScriptExecutor extends AmmoniteExecutor {
    override protected def predef: String = ""

    override def runScript(scriptPath: Path, parameters: Map[String, String], cpg: Cpg): IO[Any] =
      IO.fromTry(Try(IOUtils.readLinesInFile(scriptPath).mkString("\n")))
  }

  private object TestScriptManager extends ScriptManager(TestScriptExecutor)

  def withScriptManager(f: ScriptManager => Unit): Unit = {
    f(TestScriptManager)
  }

  "listing scripts" should {
    "be correct" in withScriptManager { scriptManager =>
      val scripts = scriptManager.scripts()
      val expected = List(
        ScriptCollections(
          "general",
          ScriptDescriptions(
            "A collection of general purpose scripts.",
            List(ScriptDescription("list-funcs.sc", "Lists all functions."))
          )
        ),
        ScriptCollections(
          "java",
          ScriptDescriptions(
            "A collection of java-specific scripts.",
            List(ScriptDescription("list-sl-ns.sc", "Lists all shiftleft namespaces."))
          )
        ),
        ScriptCollections(
          s"general${java.io.File.separator}general_plus",
          ScriptDescriptions("Even more general purpose scripts.", List.empty)
        )
      )

      scripts should contain theSameElementsAs expected
    }
  }

  "running scripts" should {
    "be correct when explicitly specifying a CPG" in withScriptManager { scriptManager =>
      val expected =
        """|@main def main() = {
           |  cpg.method.name.l
           |}""".stripMargin

      scriptManager.runScript("general/list-funcs.sc", Map.empty, Cpg.emptyCpg) shouldBe expected
    }

    "be correct when specifying a CPG filename" in withScriptManager { scriptManager =>
      val expected =
        """|@main def main() = {
           |  cpg.method.name.l
           |}""".stripMargin

      scriptManager.runScript("general/list-funcs.sc", Map.empty, DEFAULT_CPG_NAME) shouldBe expected
    }

    "throw an exception if the specified CPG can not be found" in withScriptManager { scriptManager =>
      intercept[Exception] {
        scriptManager.runScript("general/list-funcs.sc", Map.empty, "cake.bin.zip")
      }
    }

    "throw an exception if the specified script can not be found" in withScriptManager { scriptManager =>
      intercept[NoSuchFileException] {
        scriptManager.runScript("list-funcs.sc", Map.empty, Cpg.emptyCpg)
      }
    }
  }

}
