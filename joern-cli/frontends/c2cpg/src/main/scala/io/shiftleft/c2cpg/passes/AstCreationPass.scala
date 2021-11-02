package io.shiftleft.c2cpg.passes

import io.shiftleft.c2cpg.C2Cpg
import io.shiftleft.c2cpg.astcreation.{AstCreator, Defines}
import io.shiftleft.c2cpg.datastructures.Global
import io.shiftleft.c2cpg.parser.{CdtParser, HeaderFileFinder, ParserConfig}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, DiffGraph, IntervalKeyPool}
import io.shiftleft.x2cpg.SourceFiles

import java.nio.file.Paths
import scala.jdk.CollectionConverters._

class AstCreationPass(cpg: Cpg,
                      keyPool: Option[IntervalKeyPool],
                      config: C2Cpg.Config,
                      parseConfig: ParserConfig = ParserConfig.empty,
                      headerFileFinder: HeaderFileFinder = null)
    extends ConcurrentWriterCpgPass[String](cpg, keyPool = keyPool) {

  private val global: Global = new Global()

  def usedTypes(): List[String] =
    global.usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList

  override def generateParts(): Array[String] =
    SourceFiles.determine(config.inputPaths, config.sourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraph.Builder, filename: String): Unit =
    new CdtParser(parseConfig, headerFileFinder).parse(Paths.get(filename)).foreach { parserResult =>
      val localDiff = DiffGraph.newBuilder
      new AstCreator(filename, config, global, localDiff, parserResult).createAst()
      diffGraph.moveFrom(localDiff)
    }

}
