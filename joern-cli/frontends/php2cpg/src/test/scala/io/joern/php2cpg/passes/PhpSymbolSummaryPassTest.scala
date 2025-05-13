package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.utils.FileUtil
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
      { case PhpNamespace(name, (fooMethod: PhpFunction) :: Nil) :: Nil =>
        name shouldBe NamespaceTraversal.globalNamespaceName
        fooMethod.name shouldBe "foo"
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
        |""".stripMargin :: Nil,
      {
        case PhpNamespace(
              name,
              PhpClass(className, (constr: PhpFunction) :: (fooMethod: PhpFunction) :: Nil) :: Nil
            ) :: Nil =>
          name shouldBe NamespaceTraversal.globalNamespaceName
          className shouldBe "Foo"
          constr.name shouldBe Domain.ConstructorMethodName
          fooMethod.name shouldBe "foo"
      }
    )
  }

  "pre-parsing a file with a top-level class with a nested constant should provide a summary of that class" in {
    assertAgainstTempFile(
      """<?php
        |class Foo {
        |  const B = "B";
        |}
        |""".stripMargin :: Nil,
      {
        case PhpNamespace(
              name,
              PhpClass(className, (constr: PhpFunction) :: (bConst: PhpMember) :: Nil) :: Nil
            ) :: Nil =>
          name shouldBe NamespaceTraversal.globalNamespaceName
          className shouldBe "Foo"
          constr.name shouldBe Domain.ConstructorMethodName
          bConst.name shouldBe "B"
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
        case PhpNamespace(
              globalName,
              PhpNamespace(fooName, PhpNamespace(barName, PhpClass(bazName, _) :: Nil) :: Nil) :: Nil
            ) :: Nil =>
          globalName shouldBe NamespaceTraversal.globalNamespaceName
          fooName shouldBe "Foo"
          barName shouldBe "Bar"
          bazName shouldBe "Baz"
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
        case PhpNamespace(
              globalName,
              PhpNamespace(
                fooName,
                PhpNamespace(barName, PhpClass(bazName, _) :: PhpClass(fazName, _) :: Nil) :: Nil
              ) :: Nil
            ) :: Nil =>
          globalName shouldBe NamespaceTraversal.globalNamespaceName
          fooName shouldBe "Foo"
          barName shouldBe "Bar"
          bazName shouldBe "Baz"
          fazName shouldBe "Faz"
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
        case PhpNamespace(
              globalName,
              PhpNamespace(
                fooName,
                PhpClass(fazName, _) ::
                PhpNamespace(barName, PhpClass(bazName, _) :: Nil) :: Nil
              ) :: Nil
            ) :: Nil =>
          globalName shouldBe NamespaceTraversal.globalNamespaceName
          fooName shouldBe "Foo"
          barName shouldBe "Bar"
          bazName shouldBe "Baz"
          fazName shouldBe "Faz"
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
        case PhpNamespace(
              globalName,
              PhpNamespace(fooName, PhpNamespace(barName, PhpFunction(bazName) :: Nil) :: Nil) :: Nil
            ) :: Nil =>
          globalName shouldBe NamespaceTraversal.globalNamespaceName
          fooName shouldBe "Foo"
          barName shouldBe "Bar"
          bazName shouldBe "baz"
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
        case PhpNamespace(
              globalName,
              PhpNamespace(
                fooName,
                PhpClass(fazName, _) ::
                PhpNamespace(barName, PhpClass(bazName, _) :: Nil) :: Nil
              ) :: Nil
            ) :: Nil =>
          globalName shouldBe NamespaceTraversal.globalNamespaceName
          fooName shouldBe "Foo"
          barName shouldBe "Bar"
          bazName shouldBe "Baz"
          fazName shouldBe "Faz"
      }
    )
  }

}

object PhpSymbolSummaryPassTest {

  import FileUtil.PathExt

  case class ConfigAndParser(config: Config, parser: PhpParser)

  def assertAgainstTempFile(code: Seq[String], assertion: Seq[SymbolSummary] => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("php-test") { tmpDirPath =>
      code.zipWithIndex.foreach { (content, idx) =>
        val tmpFilePath = tmpDirPath / s"Test$idx.php"
        Files.createFile(tmpFilePath)
        FileUtil.writeBytes(tmpFilePath, content.getBytes)
      }
      val config = Config().withInputPath(tmpDirPath.toString)
      PhpParser.getParser(config) match {
        case Some(parser) =>
          var buffer = Option.empty[Seq[SymbolSummary]]
          new SymbolSummaryPass(config, Cpg.empty, parser, summary => buffer = Option(summary)).createAndApply()
          buffer.foreach(assertion)
        case None => Matchers.fail(s"Unable to create a PHP parser! See logs for details.")
      }
    }
  }

}
