package io.joern.kotlin2cpg.interop

import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg

object JavasrcInterop {
  def astCreationPass(inputPath: String, paths: List[String], cpg: Cpg): JavaSrcAstCreationPass = {
    val javasrcConfig = JavaSrc2Cpg.DefaultConfig.withInputPath(inputPath)
    new JavaSrcAstCreationPass(javasrcConfig, cpg, Some(paths))
  }
}
