package io.joern.swiftsrc2cpg.testfixtures

import io.joern.swiftsrc2cpg.{Config, SwiftSrc2Cpg}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil

trait SwiftCompilerSrc2CpgFrontend extends LanguageFrontend {
  final override type ConfigType = Config

  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    FileUtil.deleteOnExit(cpgOutFile)

    val pathAsString = sourceCodePath.toPath.resolve("SwiftTest").toAbsolutePath.toString
    var config = Config()
      .withInputPath(pathAsString)
      .withOutputPath(cpgOutFile.toString)
    getConfig().foreach(c => config = config.withDefines(c.defines).withSwiftBuild(c.swiftBuild))

    new SwiftSrc2Cpg().createCpg(config).get
  }

}
