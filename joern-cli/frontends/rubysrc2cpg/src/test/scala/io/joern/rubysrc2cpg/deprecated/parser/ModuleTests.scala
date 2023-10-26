package io.joern.rubysrc2cpg.deprecated.parser

class ModuleTests extends RubyParserAbstractTest {

  "Empty module definition" should {

    "be parsed as a definition" when {

      "defined in a single line" in {
        val code = """module Bar; end"""
        printAst(_.moduleDefinition(), code) shouldEqual
          """ModuleDefinition
            | module
            | ClassOrModuleReference
            |  Bar
            | BodyStatement
            |  CompoundStatement
            |   ;
            | end""".stripMargin
      }
    }
  }

}
