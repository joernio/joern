package io.joern.javasrc2cpg.querying

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

}
