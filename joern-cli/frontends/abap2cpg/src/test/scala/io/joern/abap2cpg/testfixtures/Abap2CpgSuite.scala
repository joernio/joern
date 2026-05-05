package io.joern.abap2cpg.testfixtures

import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg}

class Abap2CpgDefaultTestCpg extends DefaultTestCpg with Abap2CpgFrontend {
  // abapgen requires ABAP-standard naming conventions - override per test if needed
  override val fileSuffix: String = ".clas.abap"
}

class Abap2CpgSuite extends Code2CpgFixture(() => new Abap2CpgDefaultTestCpg())
