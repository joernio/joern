package io.joern.jssrc2cpg.io

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class TranspiledFileDetectionTests extends AstJsSrc2CpgSuite {

  "Detecting transpiled files" should {
    "skip transpiled files correctly (with source map comment)" in {
      val cpg = code(
        """
         |console.log('Hello World!');
         |//sourceMappingURL=foo
         |""".stripMargin,
        "index.js"
      )
        .moreCode("console.log('Hello World!');", "index.ts")
      cpg.file.name.l shouldBe List("index.ts")
    }

    "skip transpiled files correctly (with source map file)" in {
      val cpg = code("console.log('Hello World!');", "index.ts")
        .moreCode("console.log('Hello World!');", "index.js")
        .moreCode("", "index.js.map")
      cpg.file.name.l shouldBe List("index.ts")
    }

    "skip transpiled files correctly (with other file types)" in {
      val cpg = code("console.log('Hello World!');", "index.vue")
        .moreCode("console.log('Hello World!');", "index.ejs")
        .moreCode("console.log('Hello World!');", "index.js")
        .moreCode("", "index.js.map")
      cpg.file.name.l shouldBe List("index.ejs", "index.vue")
    }

    "not skip transpiled files when there is no source map file or comment" in {
      val cpg = code("console.log('Hello World!');", "index.ts").moreCode("console.log('Hello World!');", "index.js")
      cpg.file.name.l shouldBe List("index.js", "index.ts")
    }

    "not skip transpiled files when there is no sibling file with same name but a source map file only" in {
      val cpg = code("console.log('Hello World!');", "index.js").moreCode("", "index.js.map")
      cpg.file.name.l shouldBe List("index.js")
    }

    "not skip transpiled files when there is no sibling file with same name but a source map comment only" in {
      val cpg = code(
        """
          |console.log('Hello World!');
          |//sourceMappingURL=foo
          |""".stripMargin,
        "index.js"
      )
      cpg.file.name.l shouldBe List("index.js")
    }
  }

}
