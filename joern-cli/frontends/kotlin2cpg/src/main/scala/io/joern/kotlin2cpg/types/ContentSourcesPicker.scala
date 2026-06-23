package io.joern.kotlin2cpg.types

import java.nio.file.{Files, Paths}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil

object ContentSourcesPicker {

  // In the following directory structure:
  //  ____________________
  //  | dir1
  //  |   -> build.gradle.kts
  //  |   -> dir2
  //  |      -> build.gradle.kts
  //  |      -> dir3
  //  |        -> source1.kt
  //  |        -> source2.kt
  //  |-------------------
  //  The list of paths which are acceptable for the current version of the Kotlin compiler API is:
  //  `Seq("dir1/dir2/dir3")` and nothing else.

  def dirsForRoot(rootDir: String): Seq[String] = {
    val dir = Paths.get(rootDir)
    if (!dir.listFiles().exists(Files.isDirectory(_))) {
      return Seq(rootDir)
    }

    // Single traversal of the entire tree
    val allPaths     = dir.walk().filterNot(_ == dir).toSeq
    val allDirs      = allPaths.filter(Files.isDirectory(_))
    val dirsByParent = allDirs.groupBy(_.getParent)

    // For each .kts file, mark all ancestor dirs (within rootDir) as containing .kts
    val ktsFiles = allPaths
      .filter(path => path.hasExtension && path.toString.endsWith(".kts"))
    val dirsWithKts: Set[java.nio.file.Path] = ktsFiles.flatMap { kts =>
      val dirs =
        Iterator
          .iterate(kts.getParent)(_.getParent)
          .takeWhile(parent => parent != null && parent.startsWith(dir))
          .toSeq

      dirs
    }.toSet

    allDirs.flatMap { dir =>
      val subDirs    = dirsByParent.getOrElse(dir, Seq.empty)
      val dirsPicked = subDirs.filterNot(dirsWithKts.contains)
      if (dirsWithKts.contains(dir)) dirsPicked.map(_.toString)
      else Seq(dir.toString)
    }
  }
}
