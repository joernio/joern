package io.joern.javasrc2cpg.util

import better.files.File

import java.nio.file.Path
import scala.collection.mutable
import scala.util.matching.Regex

object PackageRootFinder {

  private val PackageNameRegex: Regex = raw"package\s+([a-zA-Z$$_.]+)\s*;".r

  def packageRootsFromFiles(inputPath: Path, relativeFilenames: List[Path]): List[Path] = {
    // Files are sorted by path length to efficiently handle the non-standard package structure case described
    // below. If the `parentDirectory` in that case is closest to the input path, then the largest number of
    // files will be removed from the `filesToCheck` list.
    var filesToCheck                              = mutable.ListBuffer.from(relativeFilenames).sortBy(_.getNameCount)
    val discoveredRoots: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    while (filesToCheck.nonEmpty) {
      val filePath = filesToCheck.remove(0)
      // If filePath.getParent is null, then parent is actually the inputPath, but in this case
      // we don't want to "break out" of the specified input directory, so just use the relative
      // path `.`, which is equivalent to the inputPath since it is relative to the input path
      val dotPath         = Path.of(".")
      val parentDirectory = Option(filePath.getParent).getOrElse(dotPath)

      SourceParser.fileIfExists(inputPath.resolve(filePath)).foreach { file =>
        val discoveredRoot = packageNameForFile(file) match {
          case Some(packageName) =>
            val packageDirs = Path.of(packageName.replaceAll(raw"\.", "/"))

            if (parentDirectory.endsWith(packageDirs))
              parentDirectory.subpath(0, parentDirectory.getNameCount - packageDirs.getNameCount)
            else
              // In this case, the package name doesn't match the given directory structure, either
              // because the project doesn't follow convention or because the given inputPath is
              // already a subdirectory of the package tree (for example, `projectRoot/src/main/java/com/`)
              filePath

          case None => filePath
        }

        discoveredRoots.addOne(discoveredRoot)
        if (discoveredRoot != dotPath) {
          filesToCheck = filesToCheck.filterNot(path => path.startsWith(discoveredRoot))
        }
      }
    }

    discoveredRoots.toList
  }

  private def packageNameForFile(file: File): Option[String] = {
    PackageNameRegex.findFirstMatchIn(file.contentAsString).map(_.group(1))
  }
}
