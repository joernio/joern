package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject.SourceUnit
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory
import spray.json._

import scala.util.{Failure, Success, Try, Using}

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[_ <: AnyRef] = filenames.toArray

  /** Creates an AST from the given JSON file.
    * @param builder
    *   the builder holding all the changes from this pass.
    * @param part
    *   the path to the Surya generated AST JSON file.
    * @return
    *   a list of changes generated from this pass.
    */
  override def runOnPart(builder: DiffGraphBuilder, part: String): Unit = {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._

    Using.resource(scala.io.Source.fromFile(part)) { source =>
      val lines = source.getLines() mkString "\n"
      Try(lines.parseJson.convertTo[SourceUnit]) match {
        case Failure(e) => logger.error(s"Unable to convert JSON to SourceUnit $part due to an exception", e)
        case Success(sourceUnit) =>
          val filePathFromTempDir = part.stripPrefix(codeDir)
          val localDiff           = new AstCreator(filePathFromTempDir, sourceUnit, global).createAst()
          builder.absorb(localDiff)
      }
    }
  }

}
