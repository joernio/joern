package io.joern.solidity2cpg.passes

import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser._
import io.joern.solidity2cpg.domain.SuryaObject.SourceUnit
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentSkipListSet
import scala.util.Using

case class Global(usedTypes: ConcurrentSkipListSet[String] = new ConcurrentSkipListSet[String]())

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ConcurrentWriterCpgPass[String](cpg, keyPool = Some(keyPool)) {

  val global: Global = Global()

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[_ <: AnyRef] = filenames.toArray

  /** Creates an AST from the given JSON file.
    * @param filename
    *   the path to the Surya generated AST JSON file.
    * @return
    *   a list of changes generated from this pass.
    */
  override def runOnPart(builder: DiffGraphBuilder, part: String): Unit = {
    Using.resource(scala.io.Source.fromFile(part)) { source =>
      val lines = source.getLines mkString "\n"

      val maybeSourceUnit = parse(lines) match {
        case Left(e) =>
          logger.error(s"Unable to parse $part", e)
        case Right(json) => json.as[SourceUnit]
      }
      maybeSourceUnit match {
        case Right(sourceUnit: SourceUnit) => new AstCreator(part, builder, global).createAst(sourceUnit)
        case Left(e) => logger.error(s"Unable to convert JSON to SourceUnit $part due to an exception", e)
        case _ => logger.error(s"Unable to convert JSON to SourceUnit $part")
      }
    }
  }

}
