package io.joern.javasrc2cpg.testfixtures

import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}

import java.io.File

class JavaSrcFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath)
  }
}

class JavaSrcCodeToCpgFixture extends CodeToCpgFixture(new JavaSrcFrontend) {}
