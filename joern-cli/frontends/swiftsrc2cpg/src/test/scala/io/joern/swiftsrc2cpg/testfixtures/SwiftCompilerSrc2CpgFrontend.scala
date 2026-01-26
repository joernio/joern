package io.joern.swiftsrc2cpg.testfixtures

import io.joern.swiftsrc2cpg.{Config, SwiftSrc2Cpg}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.validation.PostFrontendValidator

trait SwiftCompilerSrc2CpgFrontend extends LanguageFrontend {
  final override type ConfigType = Config

  def execute(sourceCodePath: java.io.File): Cpg = {
    val pathAsString = sourceCodePath.toPath.resolve("SwiftTest").toAbsolutePath.toString
    var config = Config()
      .withInputPath(pathAsString)
    getConfig().foreach(c => config = config.withDefines(c.defines).withSwiftBuild(c.swiftBuild))

    val cpg = new SwiftSrc2Cpg().createCpg(config).get
    new PostFrontendValidator(cpg, false).run()
    cpg
  }

}
