package io.joern.php2cpg.passes

import io.joern.php2cpg.datastructures.{PhpField, PhpMethod, PhpProgramSummary}
import io.joern.php2cpg.parser.PhpParser
import io.joern.php2cpg.{Config, Php2Cpg}
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class PhpSummaryPassTests extends AnyWordSpec with Matchers {

  import PhpSummaryPassTests.*

  "pre-parsing a file with a top-level function should provide a summary of that function" in {
    assertAgainstTempFile(
      """<?php
        |function foo($a, string $b): int {}
        |""".stripMargin,
      { programSummary =>
        val summary = programSummary.namespaceToType
        summary.size shouldBe 1
        val globalType = summary("Test.php:<global>").head
        globalType.name shouldBe "Test.php:<global>"
        globalType.methods should contain(
          PhpMethod("foo", List(("a", "ANY"), ("b", "string")), "int", Some("Test.php:<global>"))
        )
      }
    )
  }

  "pre-parsing a file with a top-level class with a nested function should provide a summary of that class" in {
    assertAgainstTempFile(
      """<?php
        |class Foo {
        |  final public function foo(int $x): int {
        |    return 0;
        |  }
        |}
        |""".stripMargin,
      { programSummary =>
        val summary = programSummary.namespaceToType
        summary.size shouldBe 1
        val fooType = summary("Test.php:<global>").filter(_.name == "Foo").head
        fooType.methods should contain(PhpMethod("foo", List(("this", "Foo"), ("x", "int")), "int", Some("Foo")))
        fooType.methods should contain(PhpMethod("__construct", List(("this", "Foo")), "ANY", Some("Foo")))
      }
    )
  }

  "pre-parsing a file with a top-level class with a nested constant should provide a summary of that class" in {
    assertAgainstTempFile(
      """<?php
        |class Foo {
        |  const B = "B";
        |}
        |""".stripMargin,
      { programSummary =>
        val summary = programSummary.namespaceToType
        summary.size shouldBe 1
        val fooType = summary("Test.php:<global>").filter(_.name == "Foo").head
        fooType.fields should contain(PhpField("B", "ANY"))
      }
    )
  }

}

object PhpSummaryPassTests {

  import FileUtil.PathExt

  case class ConfigAndParser(config: Config, parser: PhpParser)

  def assertAgainstTempFile(code: String, assertion: PhpProgramSummary => Assertion): Unit = {
    FileUtil.usingTemporaryDirectory("php-test") { tmpDirPath =>
      val tmpFilePath = tmpDirPath / "Test.php"
      Files.createFile(tmpFilePath)
      FileUtil.writeBytes(tmpFilePath, code.getBytes)
      val config = Config().withInputPath(tmpFilePath.toString)
      PhpParser.getParser(config) match {
        case Some(parser) =>
          new Php2Cpg().parseFiles(config, Option(parser)).map(_.programSummary).headOption match {
            case Some(summary) => assertion(summary)
            case None          => Matchers.fail(s"Unable to obtain summary from given code! See logs for details.")
          }
        case None => Matchers.fail(s"Unable to create a PHP parser! See logs for details.")
      }
    }
  }

}
