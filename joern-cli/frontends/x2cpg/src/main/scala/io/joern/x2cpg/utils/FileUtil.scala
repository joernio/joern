package io.joern.x2cpg.utils

import java.io.{BufferedOutputStream, FileNotFoundException, IOException, InputStream, OutputStream}
import java.nio.file.{
  FileAlreadyExistsException,
  Files,
  LinkOption,
  NoSuchFileException,
  Path,
  SimpleFileVisitor,
  StandardCopyOption
}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.charset.Charset
import java.time.Instant
import java.util.zip.{ZipEntry, ZipFile}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Using

object FileUtil {
  def newTemporaryFile(prefix: String = "", suffix: String = "", parent: Option[Path] = None): Path = {
    parent match {
      case Some(dir) => Files.createTempFile(dir, prefix, suffix)
      case _         => Files.createTempFile(prefix, suffix)
    }
  }

  def usingTemporaryFile[U](prefix: String = "", suffix: String = "", parent: Option[Path] = None)(
    f: Path => U
  ): Unit = {
    val file = newTemporaryFile(prefix, suffix, parent)

    try {
      f(file)
    } finally {
      delete(file, swallowIoExceptions = true)
    }
  }

  def usingTemporaryDirectory[T](prefix: String = "")(f: Path => T): T = {
    val file = Files.createTempDirectory(prefix)
    try {
      f(file)
    } finally {
      try {
        delete(file, swallowIoExceptions = true)
      } catch {
        case _: Exception => // Ensure we don't throw from finally
      }
    }
  }

  def copyFiles(from: Path, to: Path): Unit = {
    if (Files.isDirectory(to)) {
      from.copyToDirectory(to)
    } else {
      from.copyTo(to, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  def deleteOnExit(
    file: Path,
    swallowIOExceptions: Boolean = false,
    linkOptions: LinkOption = LinkOption.NOFOLLOW_LINKS
  ): Unit = {
    try {
      if (Files.isDirectory(file, linkOptions)) {
        val children = Using.resource(Files.newDirectoryStream(file)) { dirStream =>
          dirStream.iterator().asScala.toList
        }

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
        val children = Using.resource(Files.newDirectoryStream(file)) { dirStream =>
          dirStream.iterator().asScala.toList
        }

        // Delete all children first
        children.foreach { child =>
          delete(child, swallowIoExceptions, linkOptions)
        }
      }

      Files.deleteIfExists(file)
    } catch {
      case _: IOException if swallowIoExceptions => // do nothing
    }
  }

  def writeBytes(file: Path, content: Iterable[Byte], bufferSize: Int = 8192): Unit = {
    Using.Manager { use =>
      val fos = use(Files.newOutputStream(file))
      val bos = use(new BufferedOutputStream(fos))
      content.grouped(bufferSize).foreach(buffer => bos.write(buffer.toArray))
      bos.flush()
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
      Using.Manager { use =>
        val zipFile = use(new ZipFile(p.absolutePathAsString, Charset.defaultCharset()))
        val entries = zipFile.entries().asScala.filter(zipFilter)

        entries.foreach { entry =>
          val entryName = entry.getName.replace("\\", "/")
          val child = (destination / entryName).createWithParentsIfNotExists(
            asDirectory = entry.isDirectory,
            createParents = true
          )

          if (!entry.isDirectory) {
            val zipStream    = use(zipFile.getInputStream(entry))
            val outputStream = use(Files.newOutputStream(child))
            pipeTo(zipStream, outputStream, Array.ofDim[Byte](bufferSize))
          }
        }
      }

      destination
    }

    /** @return
      *   size of the directory or file
      */
    def size: Long = {
      p.walk()
        .map { f =>
          {
            try {
              Files.size(f)
            } catch {
              case (_: FileNotFoundException | _: NoSuchFileException) if Files.isDirectory(f) => 0L
            }
          }
        }
        .sum
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
