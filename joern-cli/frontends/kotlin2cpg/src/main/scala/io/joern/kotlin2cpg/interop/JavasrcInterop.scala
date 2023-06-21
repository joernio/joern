package io.joern.kotlin2cpg.interop

import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.joern.javasrc2cpg.{Config => JavaSrcConfig}
import io.joern.javasrc2cpg.passes.{SplitJpAsts, SourceFileInfo, SplitDirectories}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node.Parsedness
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.joern.javasrc2cpg.util.SourceRootFinder

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.jdk.OptionConverters.RichOptional
import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.{JavaParser, ParserConfiguration}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala
import better.files.File
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.joern.javasrc2cpg.passes.{AstCreationPass => JavaSrcAstCreationPass}
import java.nio.file.Paths

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
