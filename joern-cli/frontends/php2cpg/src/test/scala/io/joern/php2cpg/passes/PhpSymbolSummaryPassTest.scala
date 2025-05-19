package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.Assertion
import org.scalatest.Assertions.fail
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class PhpSymbolSummaryPassTest extends AnyWordSpec with Matchers {

  import PhpSymbolSummaryPassTest.*

  "pre-parsing a file with a top-level function should provide a summary of that function" in {
    assertAgainstTempFile(
      """<?php
        |function foo($a, string $b): int {}
        |""".stripMargin :: Nil,
      { case Seq("foo" -> (PhpFunction("foo") :: Nil)) => succeed }
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
        |""".stripMargin :: Nil,
      {
        case Seq(
              "Foo" -> (PhpClass("Foo") :: Nil),
              "Foo\\__construct" -> (PhpFunction("Foo\\__construct") :: Nil),
              "Foo\\foo" -> (PhpFunction("Foo\\foo") :: Nil)
            ) =>
          succeed
      }
    )
  }

  "pre-parsing a file with a nested namespace declaration should be summarized accordingly" in {
    assertAgainstTempFile(
      """<?php
        |namespace Foo\Bar;
        |class Baz {}
        |""".stripMargin :: Nil,
      {
        case Seq(
              "Foo" -> (PhpNamespace("Foo") :: Nil),
              "Foo\\Bar" -> (PhpNamespace("Foo\\Bar") :: Nil),
              "Foo\\Bar\\Baz" -> (PhpClass("Foo\\Bar\\Baz") :: Nil),
              "Foo\\Bar\\Baz\\__construct" -> (PhpFunction("Foo\\Bar\\Baz\\__construct") :: Nil)
            ) =>
          succeed
      }
    )
  }

  "pre-parsing a file with a duplicate namespace declaration should be summarized and deduplicated accordingly" in {
    assertAgainstTempFile(
      """<?php
        |namespace Foo\Bar;
        |class Baz {}
        |""".stripMargin ::
        """<?php
        |namespace Foo\Bar;
        |class Faz {}
        |""".stripMargin :: Nil,
      {
        case Seq(
              "Foo" -> (PhpNamespace("Foo") :: Nil),
              "Foo\\Bar" -> (PhpNamespace("Foo\\Bar") :: Nil),
              "Foo\\Bar\\Baz" -> (PhpClass("Foo\\Bar\\Baz") :: Nil),
              "Foo\\Bar\\Baz\\__construct" -> (PhpFunction("Foo\\Bar\\Baz\\__construct") :: Nil),
              "Foo\\Bar\\Faz" -> (PhpClass("Foo\\Bar\\Faz") :: Nil),
              "Foo\\Bar\\Faz\\__construct" -> (PhpFunction("Foo\\Bar\\Faz\\__construct") :: Nil)
            ) =>
          succeed
      }
    )
  }

  "pre-parsing a file with a nested namespace declaration should be summarized and deduplicated accordingly" in {
    assertAgainstTempFile(
      """<?php
        |namespace Foo {
        |    class Faz {}
        |    namespace Foo\Bar {
        |        class Baz {}
        |    }
        |}
        |""".stripMargin :: Nil,
      {
        case Seq(
              "Foo" -> (PhpNamespace("Foo") :: Nil),
              "Foo\\Bar" -> (PhpNamespace("Foo\\Bar") :: Nil),
              "Foo\\Bar\\Baz" -> (PhpClass("Foo\\Bar\\Baz") :: Nil),
              "Foo\\Bar\\Baz\\__construct" -> (PhpFunction("Foo\\Bar\\Baz\\__construct") :: Nil),
              "Foo\\Faz" -> (PhpClass("Foo\\Faz") :: Nil),
              "Foo\\Faz\\__construct" -> (PhpFunction("Foo\\Faz\\__construct") :: Nil)
            ) =>
          succeed
      }
    )
  }

  "pre-parsing a file with a nested function within a nested namespace should be summarized accordingly" in {
    assertAgainstTempFile(
      """<?php
        |namespace Foo {
        |    namespace Foo\Bar {
        |        function baz() {}
        |    }
        |}
        |""".stripMargin :: Nil,
      {
        case Seq(
              "Foo" -> (PhpNamespace("Foo") :: Nil),
              "Foo\\Bar" -> (PhpNamespace("Foo\\Bar") :: Nil),
              "Foo\\Bar\\baz" -> (PhpFunction("Foo\\Bar\\baz") :: Nil)
            ) =>
          succeed
      }
    )
  }

  "pre-parsing a file with separated namespaces should be summarized and deduplicated accordingly" in {
    assertAgainstTempFile(
      """<?php
        |namespace Foo {
        |    class Faz {}
        |}
        |
        |namespace Foo\Bar {
        |     class Baz {}
        |}
        |""".stripMargin :: Nil,
      {
        case Seq(
              "Foo" -> (PhpNamespace("Foo") :: Nil),
              "Foo\\Bar" -> (PhpNamespace("Foo\\Bar") :: Nil),
              "Foo\\Bar\\Baz" -> (PhpClass("Foo\\Bar\\Baz") :: Nil),
              "Foo\\Bar\\Baz\\__construct" -> (PhpFunction("Foo\\Bar\\Baz\\__construct") :: Nil),
              "Foo\\Faz" -> (PhpClass("Foo\\Faz") :: Nil),
              "Foo\\Faz\\__construct" -> (PhpFunction("Foo\\Faz\\__construct") :: Nil)
            ) =>
          succeed
      }
    )
  }

}

object PhpSymbolSummaryPassTest {

  import FileUtil.PathExt

  case class ConfigAndParser(config: Config, parser: PhpParser)

  def assertAgainstTempFile(code: Seq[String], assertion: Seq[(String, Seq[SymbolSummary])] => Assertion): Unit = {
    FileUtil.usingTemporaryDirectory("php-test") { tmpDirPath =>
      code.zipWithIndex.foreach { (content, idx) =>
        val tmpFilePath = tmpDirPath / s"Test$idx.php"
        Files.createFile(tmpFilePath)
        FileUtil.writeBytes(tmpFilePath, content.getBytes)
      }
      val config = Config().withInputPath(tmpDirPath.toString)
      PhpParser.getParser(config) match {
        case Some(parser) =>
          var buffer = Option.empty[Map[String, Seq[SymbolSummary]]]
          new SymbolSummaryPass(config, Cpg.empty, parser, summary => buffer = Option(summary)).createAndApply()
          buffer.map(_.toSeq.sortBy(_._1)).foreach { x =>
            try {
              assertion(x)
            } catch {
              case _: Exception => fail(s"Unexpected summary: $x")
            }
          }
        case None => Matchers.fail(s"Unable to create a PHP parser! See logs for details.")
      }
    }
  }

}
