package io.joern.php2cpg.passes

import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.php2cpg.{Config, Php2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files
import scala.collection.mutable

class PhpSymbolSummaryPassTest extends AnyWordSpec with Matchers {

  import PhpSymbolSummaryPassTest.*

  "pre-parsing a file with a top-level function should provide a summary of that function" in {
    assertAgainstTempFile(
      """<?php
        |function foo($a, string $b): int {}
        |""".stripMargin,
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
        |""".stripMargin,
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
        |""".stripMargin,
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

}

object PhpSymbolSummaryPassTest {

  import FileUtil.PathExt

  case class ConfigAndParser(config: Config, parser: PhpParser)

  def assertAgainstTempFile(code: String, assertion: Seq[SymbolSummary] => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("php-test") { tmpDirPath =>
      val tmpFilePath = tmpDirPath / "Test.php"
      Files.createFile(tmpFilePath)
      FileUtil.writeBytes(tmpFilePath, code.getBytes)
      val config = Config().withInputPath(tmpFilePath.toString)
      PhpParser.getParser(config) match {
        case Some(parser) =>
          val buffer = mutable.Buffer.empty[SymbolSummary]
          new SymbolSummaryPass(config, Cpg.empty, parser, buffer.append).createAndApply()
          assertion(buffer.toSeq)
        case None => Matchers.fail(s"Unable to create a PHP parser! See logs for details.")
      }
    }
  }

}
