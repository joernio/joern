package io.joern.javasrc2cpg.passes

import better.files.File
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.ProjectRoot

class ConfigFileCreationPassTests extends JavaSrcCode2CpgFixture {

  private val testConfigDir: String = ProjectRoot.relativise("joern-cli/frontends/javasrc2cpg/src/test/resources/config_tests")

  "it should find the correct config files" in {
    val foundFiles = new ConfigFileCreationPass(testConfigDir, new Cpg()).generateParts().map(_.canonicalPath)
    val absoluteConfigDir = File(testConfigDir).canonicalPath
    foundFiles should contain theSameElementsAs Array(
      s"$absoluteConfigDir/application.conf",
      s"$absoluteConfigDir/basic.jsp",
      s"$absoluteConfigDir/basic.properties",
      s"$absoluteConfigDir/routes",
      s"$absoluteConfigDir/basic.tf",
      s"$absoluteConfigDir/basic.tfvars",
      s"$absoluteConfigDir/basic.vm",
      s"$absoluteConfigDir/batis/conf.xml",
      s"$absoluteConfigDir/dwr.xml",
      s"$absoluteConfigDir/faces-config.xml",
      s"$absoluteConfigDir/nested/nested.properties",
      s"$absoluteConfigDir/struts.xml",
      s"$absoluteConfigDir/web.xml",
    )
  }

  "it should populate config files correctly" in {
    val cpg = code("""
        |public class Foo{}
	    |""".stripMargin)
      .moreCode(
        code = "key=value",
        fileName = "config.properties"
      )

    cpg.typeDecl.name("Foo").nonEmpty shouldBe true
    cpg.configFile.l match {
      case List(configFile) =>
        configFile.name shouldBe "config.properties"
        configFile.content shouldBe "key=value"

      case result => fail(s"Expected single config file but got $result")
    }
  }

}
