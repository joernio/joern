package io.joern.kotlin2cpg.interop

import better.files.File
import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import io.joern.javasrc2cpg.{Config => JavaSrcConfig}
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

object JavasrcInterop {
  def astCreationPass(paths: List[String], cpg: Cpg): JavaSrcAstCreationPass = {
    new JavaSrcAstCreationPass(JavaSrc2Cpg.DefaultConfig, cpg, Some(paths))
  }
}
