package io.joern.x2cpg.utils

import java.io.{IOException, File as JFile}
import java.nio.file.Files
import java.nio.file.Path

import better.files.File

object FileUtil {
  def newTemporaryDirectory(prefix: String = "", parent: Option[JFile] = None): JFile = {
    parent match {
      case Some(dir) => Files.createTempDirectory(dir.toPath, prefix).toFile
      case _ => Files.createTempDirectory(prefix).toFile
    }
  }

  def newTemporaryFile(prefix: String = "", suffix: String = "", parent: Option[JFile] = None): JFile = {
    parent match {
      case Some(dir) => Files.createTempFile(dir.toPath, prefix, suffix).toFile
      case _         => Files.createTempFile(prefix, suffix).toFile
    }
  }

  def usingTemporaryDirectory[U](prefix: String = "", parent: Option[JFile] = None)(f: JFile => U): Unit = {
    val file = newTemporaryDirectory(prefix, parent)

    try {
      f(file)
    } finally {
      deleteFile(file)
    }
  }

  def deleteFile(file: JFile, swallowIoExceptions: Boolean = false): Unit = {
    try {
      if (file.isDirectory) {
        file.listFiles().foreach(x => deleteFile(x, swallowIoExceptions))
      }

      Files.delete(file.toPath)
    } catch {
      case _: IOException if swallowIoExceptions => //
    }
  }

  implicit class JFileExt(f: JFile) {
    def pathAsString: String = {
      f.getPath
    }

    def /(child: String): JFile = {
      f.toPath.resolve(child).toFile
    }
  }

  implicit class PathExt(p: Path) {
    def createDirectoryIfNotExists(): Unit = {
      Files.createDirectories(p)
    }
  }

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
