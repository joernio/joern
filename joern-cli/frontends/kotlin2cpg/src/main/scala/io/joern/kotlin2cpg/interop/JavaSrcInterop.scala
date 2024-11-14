package io.joern.kotlin2cpg.interop

import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg

object JavaSrcInterop {
  def astCreationPass(inputPath: String, paths: List[String], cpg: Cpg): JavaSrcAstCreationPass = {
    val javaSrcConfig = JavaSrc2Cpg.DefaultConfig.withInputPath(inputPath)
    new JavaSrcAstCreationPass(javaSrcConfig, cpg, Some(paths))
  }
}
