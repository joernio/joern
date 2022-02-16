package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.{KtFileWithMeta}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory
import io.joern.kotlin2cpg.types.NameGenerator

import java.util.concurrent.ConcurrentHashMap

case class Global(usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]())

class AstCreationPass(
  filesWithMeta: Iterable[KtFileWithMeta],
  nameGenerator: NameGenerator,
  cpg: Cpg,
  keyPool: IntervalKeyPool
) extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filesWithMeta.size))) {
  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(getClass)

  override def partIterator: Iterator[String] = {
    filesWithMeta.map { ktFileWithMeta => ktFileWithMeta.f.getVirtualFilePath }.iterator
  }

  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    val fileWithMeta = filesWithMeta
      .filter { ktFileWithMeta =>
        ktFileWithMeta.f.getVirtualFilePath == filename
      }
      .toList
      .headOption
    fileWithMeta match {
      case Some(fm) =>
        val diffGraph =
          new AstCreator(fm, nameGenerator, global).createAst()
        logger.debug("AST created for file at `" + filename + "`.")
        diffGraph
      case None =>
        logger.info("Could not find file at `" + filename + "`.")
        Iterator[DiffGraph]()
    }
  }
}
