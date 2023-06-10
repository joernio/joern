package io.joern.rubysrc2cpg.passes
import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.astcreation.antlr.AntlrBasedAstCreator
import io.joern.rubysrc2cpg.astcreation.jruby.JRubyBasedAstCreator
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.EnumerationHasAsScala

class AstCreationPass(cpg: Cpg, config: Config) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                        = LoggerFactory.getLogger(this.getClass)
  val global                                = new Global()
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  def allUsedTypes(): List[String] = global.usedTypes.keys().asScala.toList

  override def generateParts(): Array[String] = SourceFiles.determine(config.inputPath, RubySourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    val astCreator = if (config.useJRuby) new JRubyBasedAstCreator(fileName) else new AntlrBasedAstCreator(fileName, global)
    diffGraph.absorb(astCreator.createAst())
  }
}
