package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CodeDumperTests extends JavaSrcCode2CpgFixture {

  "a Java source code CPG" should {

    implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

    val cpg = code("""
        |public class Foo {
        |
        | public void test() {
        |   var a = 1;
        |   var b = 2;
        |   var c = a + b;
        | }
        |
        |}
        |""".stripMargin).withConfig(JavaSrc2Cpg.DefaultConfig.withDisableFileContent(false))

    "allow one to dump a method node's source code from `File.contents`" in {
      inside(cpg.method.nameExact("test").dumpRaw.l) {
        case content :: Nil =>
          content.linesIterator.map(_.strip).l shouldBe List(
            "public void test() { /* <=== Foo.test:void() */",
            "var a = 1;",
            "var b = 2;",
            "var c = a + b;",
            "}"
          )
        case content => fail(s"Expected exactly 1 content dump, but got: $content")
      }
    }

  }

}
