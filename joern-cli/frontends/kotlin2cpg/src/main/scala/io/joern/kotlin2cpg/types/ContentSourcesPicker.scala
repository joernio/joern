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
    val dir        = Paths.get(rootDir)
    val hasSubDirs = dir.listFiles().exists(Files.isDirectory(_))
    if (!hasSubDirs) {
      return Seq(rootDir)
    }
    dir
      .walk()
      .filterNot(_ == dir)
      .filter(Files.isDirectory(_))
      .flatMap { f =>
        val hasKtsFile = f.walk().filterNot(_ == f).exists { f => f.hasExtension && f.toString.endsWith(".kts") }
        val dirsPicked = f.listFiles().filter(Files.isDirectory(_)).filterNot { d =>
          d.walk().filterNot(_ == d).filter(_.hasExtension).exists(_.toString.endsWith(".kts"))
        }
        if (hasKtsFile) Some(dirsPicked.map(_.toString))
        else Some(Seq(f.toString))
      }
      .flatten
      .toSeq
  }
}
