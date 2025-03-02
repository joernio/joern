package io.joern.x2cpg.utils

import java.io.{IOException, File as JFile}
import java.nio.file.{FileAlreadyExistsException, Files, LinkOption, Path, SimpleFileVisitor}
import better.files.File

import java.nio.file.attribute.BasicFileAttributes

object FileUtil {
  // TODO: Replace better.files with this method
  def usingTemporaryDirectory[U](prefix: String = "")(f: Path => U): Unit = {
    val file = Files.createTempDirectory(prefix)

    try {
      f(file)
    } finally {
      delete(file)
    }
  }

  def deleteOnExit(file: Path, swallowIOExceptions: Boolean = false): Unit = {
    try {
      if (Files.isDirectory(file)) {
        file.toFile.listFiles().foreach(x => deleteOnExit(x.toPath, swallowIOExceptions))
      }

      file.toFile.deleteOnExit()
    } catch {
      case _: IOException if swallowIOExceptions => //
    }
  }

  def delete(
    file: Path,
    swallowIoExceptions: Boolean = false,
    linkOptions: LinkOption = LinkOption.NOFOLLOW_LINKS
  ): Unit = {
    try {
      if (Files.isDirectory(file, linkOptions)) {
        file.toFile.listFiles().foreach(x => delete(x.toPath, swallowIoExceptions, linkOptions))
      }

      Files.delete(file)
    } catch {
      case _: IOException if swallowIoExceptions => //
    }
  }

  implicit class PathExt(p: Path) {
    def /(child: String): Path = {
      p.resolve(child)
    }

    def copyToDirectory(destination: Path): Unit = {
      require(Files.isDirectory(destination), s"${destination} must be a directory")
      copyTo(destination / p.getFileName.toString)
    }

    def copyTo(destination: Path): Unit = {
      if (Files.isDirectory(p)) { // TODO: maxDepth?
        Files.walkFileTree(
          p,
          new SimpleFileVisitor[Path] {
            def newPath(subPath: Path): Path = destination.resolve(p.relativize(subPath))

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
        Files.copy(p, destination)
      }
    }

    def createWithParentsIfNotExists(asDirectory: Boolean = false, createParents: Boolean = false): Unit = {
      if (!Files.exists(p)) {
        if (asDirectory) {
          try {
            Files.createDirectories(p)
          } catch {
            case _: FileAlreadyExistsException if Files.isDirectory(p) => // do nothing
          }
        } else {
          if (createParents) Files.createDirectories(p.getParent)
          try {
            Files.createFile(p)
          } catch {
            case _: FileAlreadyExistsException if Files.isRegularFile(p) => // do nothing
          }
        }
      }
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
