package io.joern.solidity2cpg.passes

import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser._
import io.joern.solidity2cpg.domain.SuryaObject.SourceUnit
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentSkipListSet
import scala.util.Using

case class Global(usedTypes: ConcurrentSkipListSet[String] = new ConcurrentSkipListSet[String]())

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filenames.size))) {

  val global: Global = Global()

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def partIterator: Iterator[String] = filenames.iterator

  /** Creates an AST from the given JSON file.
    * @param filename
    *   the path to the Surya generated AST JSON file.
    * @return
    *   a list of changes generated from this pass.
    */
  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    Using.resource(scala.io.Source.fromFile(filename)) { source =>
      val lines = source.getLines mkString "\n"

      val maybeSourceUnit = parse(lines) match {
        case Left(e) =>
          logger.error(s"Unable to parse $filename", e)
          return Iterator()
        case Right(json) => json.as[SourceUnit]
      }
      maybeSourceUnit match {
        case Left(e) =>
          logger.error(s"Unable to convert JSON to SourceUnit $filename", e)
          Iterator()
        case Right(sourceUnit) => new AstCreator(filename, global).createAst(sourceUnit)
      }
    }
  }

}
