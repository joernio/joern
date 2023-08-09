package io.joern.kotlin2cpg.interop

import better.files.File
import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import io.joern.javasrc2cpg.{Config => JavaSrcConfig}
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

object JavasrcInterop {
  def astCreationPass(inputPath: String, paths: List[String], cpg: Cpg): JavaSrcAstCreationPass = {
    val javasrcConfig = JavaSrc2Cpg.DefaultConfig.withInputPath(inputPath)
    new JavaSrcAstCreationPass(javasrcConfig, cpg, Some(paths))
  }
}
