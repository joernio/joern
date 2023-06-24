package io.joern.kotlin2cpg.interop

import better.files.File
import io.joern.javasrc2cpg.passes.{SplitDirectories, SplitJpAsts}
import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import io.joern.javasrc2cpg.{Config => JavaSrcConfig}
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

object JavasrcInterop {
  private val logger = LoggerFactory.getLogger(getClass)

  val frontendConfig = JavaSrcConfig()

  def astCreationPass(paths: List[String], cpg: Cpg): JavaSrcAstCreationPass = {
    val javaSrcConfig  = JavaSrcConfig()
    val javaParserAsts = JavasrcInterop.javaParserAsts(paths, javaSrcConfig.inputPath)
    new JavaSrcAstCreationPass(javaSrcConfig, cpg, Some(javaParserAsts))
  }

  private def javaParserAsts(paths: Seq[String], inputPath: String): SplitJpAsts = {
    paths.foldLeft(SplitJpAsts(List(), List())) { (acc, p) =>
      val splitDirectories = SplitDirectories(p, p)
      val splitParserAsts  = JavaSrcAstCreationPass.getSplitJavaparserAsts(splitDirectories)
      SplitJpAsts(acc.analysisAsts ++ splitParserAsts.analysisAsts, acc.typesAsts ++ splitParserAsts.typesAsts)
    }
  }
}
