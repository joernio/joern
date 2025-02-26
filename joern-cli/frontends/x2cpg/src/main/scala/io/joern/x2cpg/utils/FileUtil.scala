package io.joern.x2cpg.utils

import java.io.{IOException, File as JFile}
import java.nio.file.{FileAlreadyExistsException, Files, Path, SimpleFileVisitor}
import better.files.File

import java.nio.file.attribute.BasicFileAttributes

object FileUtil {
  def newTemporaryDirectory(prefix: String = "", parent: Option[JFile] = None): JFile = {
    parent match {
      case Some(dir) => Files.createTempDirectory(dir.toPath, prefix).toFile
      case _         => Files.createTempDirectory(prefix).toFile
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

  def deleteFileOnExit(file: JFile, swallowIOExceptions: Boolean = false): Unit = {
    try {
      if (file.isDirectory) {
        file.listFiles().foreach(x => deleteFileOnExit(x, swallowIOExceptions))
      }

      Files.delete(file.toPath)
    } catch {
      case _: IOException if swallowIOExceptions => //
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
      f.toPath.toString
    }

    def /(child: String): JFile = {
      f.toPath.resolve(child).toFile
    }

    def copyToDirectory(destination: JFile): Unit = {
      require(Files.isDirectory(destination.toPath), s"${destination.getPath} must be a directory")
      copyTo(destination / f.toPath.getFileName.toString)
    }

    def copyTo(destination: JFile): Unit = {
      if (Files.isDirectory(f.toPath)) { // TODO: maxDepth?
        Files.walkFileTree(
          f.toPath,
          new SimpleFileVisitor[Path] {
            def newPath(subPath: Path): Path = destination.toPath.resolve(f.toPath.relativize(subPath))

            override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
              Files.createDirectories(newPath(dir))
              super.preVisitDirectory(dir, attrs)
            }

            override def visitFile(file: Path, attrs: BasicFileAttributes) = {
              Files.copy(file, newPath(file))
              super.visitFile(file, attrs)
            }
          }
        )
      } else {
        Files.copy(f.toPath, destination.toPath)
      }
    }

    def createIfNotExists(asDirectory: Boolean = false, createParents: Boolean = false): Unit = {
      if (Files.exists(f.toPath)) {
        // do nothing
      } else if (asDirectory) {} else {
        if (createParents) createDirectories(createParents)
        try {
          Files.createFile(f.toPath)
        } catch {
          case _: FileAlreadyExistsException if Files.isRegularFile(f.toPath) => // do nothing
        }
      }
    }

    def createDirectories(createParents: Boolean = false): Unit = {
      try {
        if (createParents) {
          Files.createDirectories(f.toPath.getParent)
        } else {
          Files.createDirectories(f.toPath)
        }
      } catch {
        case _: FileAlreadyExistsException if Files.isDirectory(f.toPath) => // Do nothing
      }
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
