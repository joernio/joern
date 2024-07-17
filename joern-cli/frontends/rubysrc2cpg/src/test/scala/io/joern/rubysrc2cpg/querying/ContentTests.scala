package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ContentTests extends RubyCode2CpgFixture(disableFileContent = false) {
  "Content of file" in {
    val fileContent =
      """
        |class Animal
        |end
        |
        |def foo
        | puts "a"
        |end
        |""".stripMargin

    val cpg = code(fileContent, "Test0.rb")

    cpg.file.name("Test0.rb").content.head shouldBe fileContent
  }

  "Content of method" in {

    val fooFunc =
      """def foo
        | puts "a"
        |end""".stripMargin

    val cpg = code(s"""$fooFunc""".stripMargin)

    val method = cpg.method.name("foo").head

    method.content.head shouldBe fooFunc
  }

  "Content of Class" in {
    val cls =
      """class Animal
        |end""".stripMargin

    val cpg    = code(s"""$cls""".stripMargin)
    val animal = cpg.typeDecl.name("Animal").head

    animal.content.head shouldBe cls
  }

  "Content of Module" in {
    val mod = """module Foo
        |end""".stripMargin

    val cpg    = code(mod)
    val module = cpg.typeDecl.name("Foo").head

    module.content.head shouldBe mod
  }

  "Method and Class content" in {
    val cls =
      """class Animal
        |end""".stripMargin

    val fooFunc =
      """def foo
        | puts "a"
        |end""".stripMargin

    val cpg = code(s"""$cls
    |$fooFunc""".stripMargin)

    val method = cpg.method.name("foo").head
    val animal = cpg.typeDecl.name("Animal").head

    method.content.head shouldBe fooFunc
    animal.content.head shouldBe cls
  }
}
