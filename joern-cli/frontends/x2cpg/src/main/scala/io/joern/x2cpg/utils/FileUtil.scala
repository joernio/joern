package io.joern.x2cpg.utils

import better.files.File

object FileUtil {

  implicit class FileExt(f: File) {

    /** Moves this file's contents, if it's a directory, or simply copies the file, and merges it with the given file,
      * if it's a directory (will throw an exception otherwise). Performs a similar operation to
      * [[java.nio.file.Files.move]], however, unlike the aforementioned function, will handle the case where the
      * destination directory is non-empty.
      *
      * @param directory
      *   the destination directory to move into.
      */
    def mergeDirectory(directory: File)(implicit
      linkOptions: File.LinkOptions = File.LinkOptions.default,
      copyOptions: File.CopyOptions = File.CopyOptions(overwrite = false)
    ): Unit = {
      require(directory.isDirectory(linkOptions), s"$directory must be a directory")

      f.walk().filter(_.isRegularFile).foreach { x =>
        val relativePath = x.pathAsString.stripPrefix(s"${f.pathAsString}${java.io.File.separator}")
        val target       = directory / relativePath
        target.parent.createDirectoryIfNotExists(createParents = true)
        x.moveTo(target)(copyOptions)
      }
    }

  }

}
