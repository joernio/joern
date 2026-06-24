package io.joern.x2cpg.utils.dependency

import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** Materializes the `classes.jar` inside an Android Archive (.aar) so the file can be consumed like any other jar. Aars
  * that don't contain a `classes.jar` are dropped (None).
  *
  * Scratch unzipping happens inside `cacheDir` and is cleaned up unconditionally. The extracted jar is stored in
  * `cacheDir` under a deterministic name derived from the aar's absolute path, so repeated calls are disk-idempotent:
  * if the jar already exists, it is returned without re-unzipping. The original .aar file is never touched.
  */
private[joern] object AarExtractor {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val aarExtension = "aar"
  private val jarInsideAar = "classes.jar"

  /** Extracts `classes.jar` from `aar` into `cacheDir`. Returns the path to the materialized jar, or None if no
    * `classes.jar` was found or extraction failed.
    */
  def extractClassesJar(aar: Path, cacheDir: Path): Option[Path] = {
    if (!aar.toString.endsWith("." + aarExtension)) {
      logger.debug(s"Refusing to extract non-aar path: $aar")
      return None
    }

    Files.createDirectories(cacheDir)
    val outFile = cacheDir.resolve(cacheKey(aar))
    if (Files.exists(outFile)) {
      logger.trace(s"Reusing previously-extracted classes.jar at $outFile for $aar")
      return Some(outFile)
    }

    val scratch = Files.createTempDirectory(cacheDir, "scratch-")
    try {
      Try(aar.unzipTo(scratch)) match {
        case Failure(ex) =>
          logger.warn(s"Failed to unzip aar at $aar: ${ex.getMessage}")
          None
        case Success(_) =>
          val classesJarCandidates = Try {
            Files
              .walk(scratch)
              .iterator()
              .asScala
              .filterNot(_ == scratch)
              .filter(_.fileName == jarInsideAar)
              .toList
          }.getOrElse(Nil)

          classesJarCandidates match {
            case classesJar :: Nil =>
              try classesJar.copyTo(outFile)
              catch {
                case _: FileAlreadyExistsException =>
                // Another caller raced us to the same cache slot — fine, the
                // existing file is the same content.
              }
              Some(outFile)
            case _ =>
              logger.debug(s"Found aar file without exactly one `classes.jar` inside at $aar")
              None
          }
      }
    } finally {
      if (Files.exists(scratch)) FileUtil.delete(scratch)
    }
  }

  /** Replaces every `.aar` entry in `paths` with the path to its extracted `classes.jar`. Non-aar entries pass through
    * unchanged. Aar entries that cannot be extracted are dropped, with a logged warning.
    */
  def materializeJars(paths: Iterable[Path], cacheDir: Path): Set[Path] = {
    paths.flatMap { path =>
      if (path.toString.endsWith("." + aarExtension)) extractClassesJar(path, cacheDir)
      else Some(path)
    }.toSet
  }

  /** Deterministic per-aar cache filename. The basename keeps results human-readable in the cache directory; the path
    * hash disambiguates aars that share a filename (e.g. multiple versions of the same artifact under different Gradle
    * cache slots).
    */
  private def cacheKey(aar: Path): String = {
    val absolute = aar.toAbsolutePath.toString
    val hash = MessageDigest
      .getInstance("SHA-1")
      .digest(absolute.getBytes(StandardCharsets.UTF_8))
    val hex  = hash.take(4).map(byteVal => f"${byteVal & 0xff}%02x").mkString
    val stem = aar.fileName.stripSuffix("." + aarExtension)
    s"$stem-$hex.jar"
  }
}
