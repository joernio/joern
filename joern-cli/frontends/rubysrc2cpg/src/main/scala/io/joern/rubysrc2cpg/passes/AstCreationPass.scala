package io.joern.rubysrc2cpg.passes
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

class AstCreationPass(inputPath: String, cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                        = LoggerFactory.getLogger(this.getClass)
  val global                                = new Global()
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, RubySourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    diffGraph.absorb(new AstCreator(fileName, global).createAst())
  }
}
