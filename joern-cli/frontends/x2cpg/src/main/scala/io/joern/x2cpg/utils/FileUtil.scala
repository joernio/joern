package io.joern.x2cpg.utils

import java.io.{IOException, InputStream, OutputStream}
import java.nio.file.{FileAlreadyExistsException, Files, LinkOption, Path, SimpleFileVisitor, StandardCopyOption}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.charset.Charset
import java.time.Instant
import java.util.zip.{ZipEntry, ZipFile}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

object FileUtil {
  def newTemporaryFile(prefix: String = "", suffix: String = ""): Path = {
    Files.createTempFile(prefix, suffix)
  }

  // TODO: Replace better.files with this method
  def usingTemporaryDirectory[U](prefix: String = "")(f: Path => U): Unit = {
    val file = Files.createTempDirectory(prefix)

    try {
      f(file)
    } finally {
      delete(file)
    }
  }

  def deleteOnExit(
    file: Path,
    swallowIOExceptions: Boolean = false,
    linkOptions: LinkOption = LinkOption.NOFOLLOW_LINKS
  ): Unit = {
    try {
      if (Files.isDirectory(file, linkOptions)) {
        val dirStream = Files.newDirectoryStream(file)
        val children  = dirStream.iterator().asScala.toList
        dirStream.close()
        children.foreach { x =>
          deleteOnExit(x, swallowIOExceptions, linkOptions)
        }
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
        val dirStream = Files.newDirectoryStream(file)
        val children  = dirStream.iterator().asScala.toList
        dirStream.close()
        children.foreach { x =>
          delete(x, swallowIoExceptions, linkOptions)
        }

      }

      Files.deleteIfExists(file)
    } catch {
      case _: IOException if swallowIoExceptions => //
    }
  }

  implicit class PathExt(p: Path) {
    def absolutePathAsString: String = p.toAbsolutePath.toString

    def /(child: String): Path = {
      p.resolve(child)
    }

    def copyToDirectory(destination: Path): Unit = {
      require(Files.isDirectory(destination), s"${destination} must be a directory")
      copyTo(destination / p.getFileName.toString)
    }

    def copyTo(destination: Path, copyOption: StandardCopyOption = StandardCopyOption.COPY_ATTRIBUTES): Unit = {
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
              Files.copy(file, newPath(file), copyOption)
              super.visitFile(file, attrs)
            }
          }
        )
      } else {
        Files.copy(p, destination, copyOption)
      }
    }

    def createWithParentsIfNotExists(asDirectory: Boolean = false, createParents: Boolean = false): Path = {
      if (!Files.exists(p)) {
        if (asDirectory) {
          try {
            Files.createDirectories(p)
          } catch {
            case _: FileAlreadyExistsException if Files.isDirectory(p) => // do nothing
          }
          p
        } else {
          if (createParents) Files.createDirectories(p.getParent)
          try {
            Files.createFile(p)
          } catch {
            case _: FileAlreadyExistsException if Files.isRegularFile(p) => // do nothing
          }
          p
        }
      } else {
        p
      }
    }

    def mergeDirectory(directory: Path, copyOptions: StandardCopyOption = StandardCopyOption.COPY_ATTRIBUTES): Unit = {
      require(Files.isDirectory(directory), s"$directory must be a directory")

      p.walk().filter(Files.isRegularFile(_)).foreach { x =>
        val relativePath = x.toString.stripPrefix(s"${p.toString}${java.io.File.separator}")
        val target       = directory / relativePath
        target.getParent.createWithParentsIfNotExists(asDirectory = true, createParents = true)
        Files.move(x, target, copyOptions)
      }
    }

    def unzipTo(
      destination: Path,
      zipFilter: ZipEntry => Boolean = _ => true,
      bufferSize: Int = 8192
    ): destination.type = {
      val zipFile = new ZipFile(p.toAbsolutePath.toString, Charset.defaultCharset())
      val entries = zipFile.entries().asScala.filter(zipFilter)

      entries.foreach { entry =>
        val entryName = entry.getName.replace("\\", "/") // see https://github.com/pathikrit/better-files/issues/262
        val child =
          (destination / entryName).createWithParentsIfNotExists(asDirectory = entry.isDirectory, createParents = true)

        if (!entry.isDirectory) {
          val inputStream  = zipFile.getInputStream(entry)
          val outputStream = Files.newOutputStream(child)
          pipeTo(inputStream, outputStream, Array.ofDim[Byte](bufferSize))

          inputStream.close()
          outputStream.close()
        }
      }

      zipFile.close()
      destination
    }

    def listFiles(): Iterator[Path] = {
      Files.list(p).iterator().asScala
    }

    def walk(): Iterator[Path] = {
      Files.walk(p).iterator().asScala
    }

    def extension: Option[String] = {
      if ((Files.isRegularFile(p) || Files.notExists(p)) && p.getFileName.toString.contains(".")) {
        val dotIdx = p.getFileName.toString.lastIndexOf(".")
        Some(p.getFileName.toString.substring(dotIdx).toLowerCase)
      } else {
        None
      }
    }
    // Taken from better.files implementation
    @tailrec final def pipeTo(in: InputStream, out: OutputStream, buffer: Array[Byte]): OutputStream = {
      val n = in.read(buffer)
      if (n > 0) {
        out.write(buffer, 0, n)
        pipeTo(in, out, buffer)
      } else {
        out
      }
    }

  }
}
