package io.joern.javasrc2cpg.util

import java.nio.file.Path
import scala.collection.mutable

object PackageRootFinder {

  def packageRootsFromFiles(inputPath: Path, fileInfos: List[SourceParser.FileInfo]): List[Path] = {
    // Files are sorted by path length to efficiently handle the non-standard package structure case described
    // below. If the `parentDirectory` in that case is closest to the input path, then the largest number of
    // files will be removed from the `filesToCheck` list.
    var filesToCheck = mutable.ListBuffer.from(fileInfos).sortBy(_.relativePath.getNameCount)
    val discoveredRoots: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    while (filesToCheck.nonEmpty) {
      val fileInfo = filesToCheck.remove(0)
      // If filePath.getParent is null, then parent is actually the inputPath, but in this case
      // we don't want to "break out" of the specified input directory, so just use the relative
      // path `.`, which is equivalent to the inputPath since it is relative to the input path
      val dotPath         = Path.of(".")
      val parentDirectory = Option(fileInfo.relativePath.getParent).getOrElse(dotPath)

      val discoveredRoot = fileInfo.packageName match {
        case Some(packageName) =>
          val packageDirs = Path.of(packageName.replaceAll(raw"\.", "/"))

          if (parentDirectory.endsWith(packageDirs))
            parentDirectory.subpath(0, parentDirectory.getNameCount - packageDirs.getNameCount)
          else
            // In this case, the package name doesn't match the given directory structure, either
            // because the project doesn't follow convention or because the given inputPath is
            // already a subdirectory of the package tree (for example, `projectRoot/src/main/java/com/`)
            fileInfo.relativePath

        case None => fileInfo.relativePath
      }

      discoveredRoots.addOne(discoveredRoot)
      if (discoveredRoot != dotPath) {
        filesToCheck = filesToCheck.filterNot(info => info.relativePath.startsWith(discoveredRoot))
      }
    }

    discoveredRoots.toList
  }

}
