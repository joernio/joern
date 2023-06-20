package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, TestCpg}
import io.shiftleft.semanticcpg.language._

class ImportsPassTests extends Code2CpgFixture(() => new TestCpgWithoutDataFlow()) {

  "ImportsPass" should {
    "create IMPORT node for declaration" in {
      val cpg = code("""
          |var barOrBaz = require('./bar.js');
          |""".stripMargin)

      val List(x) = cpg.imports.l
      x.importedEntity shouldBe Some("./bar.js")
      x.importedAs shouldBe Some("barOrBaz")
      val List(call) = x.call.l
      call.code shouldBe "require('./bar.js')"
      val List(assignment) = call.inAssignment.l
      assignment.code shouldBe "var barOrBaz = require('./bar.js')"
      assignment.target.code shouldBe "barOrBaz"
      val source = assignment.source
      source shouldBe call
    }

    "create IMPORT node for assignment from require" in {
      val cpg = code("""
          |barOrBaz = require('./bar.js');
          |""".stripMargin)
      val List(x) = cpg.imports.l
      x.importedEntity shouldBe Some("./bar.js")
      x.importedAs shouldBe Some("barOrBaz")
      val List(call) = x.call.l
      call.code shouldBe "require('./bar.js')"
      val List(assignment) = call.inAssignment.l
      assignment.code shouldBe "barOrBaz = require('./bar.js')"
      assignment.target.code shouldBe "barOrBaz"
      val source = assignment.source
      source shouldBe call
    }
  }
}

class TestCpgWithoutDataFlow extends TestCpg with JsSrc2CpgFrontend {
  override val fileSuffix: String = ".js"
  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    JsSrc2Cpg.postProcessingPasses(this).foreach(_.createAndApply())
  }
}
