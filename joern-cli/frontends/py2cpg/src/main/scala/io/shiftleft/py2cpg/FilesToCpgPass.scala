package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object FilesToCpgPass {
  private val logger = LoggerFactory.getLogger(getClass)
}

class FilesToCpgPass(cpg: Cpg, sourceFiles: Iterable[Path], keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Path](cpg, keyPools = Some(keyPool.split(sourceFiles.size))) {
  import FilesToCpgPass._

  override def partIterator: Iterator[Path] = sourceFiles.iterator

  override def runOnPart(sourceFile: Path): Iterator[DiffGraph] = {
    try {
      val content = Files.readAllBytes(sourceFile)
      val contentStr = new String(content, StandardCharsets.UTF_8)

      new CodeToCpg(contentStr).convert()
    } catch {
      case exception: Throwable =>
        logger.error(s"Failed to convert $sourceFile.", exception)
        Iterator.empty
    }
  }
}
