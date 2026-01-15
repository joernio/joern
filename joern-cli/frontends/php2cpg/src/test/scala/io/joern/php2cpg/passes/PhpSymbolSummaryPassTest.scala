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
      { case PhpFunction("foo") :: Nil => succeed }
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
      { case PhpClass("Foo") :: Nil =>
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
        case PhpNamespace("Foo") ::
            PhpNamespace("Foo\\Bar") ::
            PhpClass("Foo\\Bar\\Baz") :: Nil =>
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
        case PhpNamespace("Foo") ::
            PhpNamespace("Foo\\Bar") ::
            PhpClass("Foo\\Bar\\Baz") ::
            PhpClass("Foo\\Bar\\Faz") :: Nil =>
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
        case PhpNamespace("Foo") ::
            PhpNamespace("Foo\\Bar") ::
            PhpClass("Foo\\Bar\\Baz") ::
            PhpClass("Foo\\Faz") :: Nil =>
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
        case PhpNamespace("Foo") ::
            PhpNamespace("Foo\\Bar") ::
            PhpFunction("Foo\\Bar\\baz") :: Nil =>
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
        case PhpNamespace("Foo") ::
            PhpNamespace("Foo\\Bar") ::
            PhpClass("Foo\\Bar\\Baz") ::
            PhpClass("Foo\\Faz") :: Nil =>
          succeed
      }
    )
  }

}

object PhpSymbolSummaryPassTest {

  import FileUtil.PathExt

  case class ConfigAndParser(config: Config, parser: PhpParser)

  def assertAgainstTempFile(code: Seq[String], assertion: Seq[SymbolSummary] => Assertion): Unit = {
    FileUtil.usingTemporaryDirectory("php-test") { tmpDirPath =>
      code.zipWithIndex.foreach { (content, idx) =>
        val tmpFilePath = tmpDirPath / s"Test$idx.php"
        Files.createFile(tmpFilePath)
        FileUtil.writeBytes(tmpFilePath, content.getBytes)
      }
      val config = Config().withInputPath(tmpDirPath.toString)
      PhpParser
        .withParser(config) { parser =>
          var buffer = Option.empty[Map[String, Seq[SymbolSummary]]]
          new SymbolSummaryPass(config, Cpg.empty, parser, summary => buffer = Option(summary)).createAndApply()
          buffer.map(_.valuesIterator.flatten.toSeq.sortBy(_.name)).foreach { x =>
            try {
              assertion(x)
            } catch {
              case _: Exception => fail(s"Unexpected summary: $x")
            }
          }
        }
        .getOrElse(Matchers.fail(s"Unable to create a PHP parser! See logs for details."))
    }
  }

}
