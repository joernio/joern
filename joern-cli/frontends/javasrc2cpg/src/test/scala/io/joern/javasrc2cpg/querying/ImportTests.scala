package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImportTests extends JavaSrcCode2CpgFixture {

  "fully defined imports" should {
    lazy val cpg = code(
      """
        |package org.codeminers.controller;
        |
        |import org.codeminers.thirdparty.ThirdParty;
        |import org.codeminers.thirdparty.util.*;
        |
        |public class Controller {
        |
        |    public void foo() {
        |        Request request = new Request();
        |        ThirdParty.getSgClient().api(request);
        |    }
        |}""".stripMargin,
      fileName = "Controller.java"
    )

    "have specific namespaces represented correctly via an import node" in {
      val List(thirdParty, asterix) = cpg.imports.l
      thirdParty.importedAs shouldBe Some("ThirdParty")
      thirdParty.importedEntity shouldBe Some("org.codeminers.thirdparty.ThirdParty")
      thirdParty.code shouldBe "import org.codeminers.thirdparty.ThirdParty"

      asterix.importedAs shouldBe Some("*")
      asterix.importedEntity shouldBe Some("org.codeminers.thirdparty.util")
      asterix.code shouldBe "import org.codeminers.thirdparty.util.*"
    }

    "have correct line and column numbers for import nodes" in {
      val List(thirdParty, asterix) = cpg.imports.l
      thirdParty.lineNumber shouldBe Some(4)
      thirdParty.columnNumber shouldBe Some(1)

      asterix.lineNumber shouldBe Some(5)
      asterix.columnNumber shouldBe Some(1)
    }
  }

  "fully defined imports with file content enabled" should {
    lazy val cpg = code(
      """
        |package org.codeminers.controller;
        |
        |import org.codeminers.thirdparty.ThirdParty;
        |import org.codeminers.thirdparty.util.*;
        |
        |public class Controller {
        |
        |    public void foo() {
        |        Request request = new Request();
        |        ThirdParty.getSgClient().api(request);
        |    }
        |}""".stripMargin,
      fileName = "Controller.java"
    ).withConfig(Config().withDisableFileContent(false))

    "have correct offsets for import nodes" in {
      val List(thirdParty, asterix) = cpg.imports.l
      val fileContent               = cpg.file.head.content

      fileContent.substring(thirdParty.offset.get, thirdParty.offsetEnd.get) shouldBe
        "import org.codeminers.thirdparty.ThirdParty;"
      fileContent.substring(asterix.offset.get, asterix.offsetEnd.get) shouldBe
        "import org.codeminers.thirdparty.util.*;"
    }
  }

  "an import with extra whitespace between `import` and the package name" should {
    lazy val cpg = code(
      """
        |package org.codeminers.controller;
        |
        |import    foo.bar;
        |
        |public class Controller {
        |}""".stripMargin,
      fileName = "Controller.java"
    ).withConfig(Config().withDisableFileContent(false))

    "have the expected properties" in {
      val List(importNode) = cpg.imports.l
      importNode.importedAs shouldBe Some("bar")
      importNode.importedEntity shouldBe Some("foo.bar")
      // code is re-printed by javaparser, so extra whitespace is normalized to a single space
      importNode.code shouldBe "import foo.bar"
      importNode.lineNumber shouldBe Some(4)
      importNode.columnNumber shouldBe Some(1)

      // offset/offsetEnd are derived from the original source positions, so the extra whitespace is preserved
      val fileContent = cpg.file.head.content
      fileContent.substring(importNode.offset.get, importNode.offsetEnd.get) shouldBe "import    foo.bar;"
    }
  }

}
