package io.joern.javasrc2cpg.testfixtures

import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, CodeToCpgFixture, LanguageFrontend}

import java.io.File

class JavaSrcFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class JavaSrcCodeToCpgFixture extends CodeToCpgFixture(new JavaSrcFrontend) {}

class JavaSrcCode2CpgFixture extends Code2CpgFixture(new JavaSrcFrontend)
