package io.joern.jimple2cpg.util

import better.files.*
import io.joern.x2cpg.SourceFiles
import org.objectweb.asm.ClassReader.SKIP_CODE
import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import org.slf4j.LoggerFactory

import java.io.FileInputStream
import java.nio.file.Path
import java.util.zip.ZipEntry
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.{Failure, Left, Success, Try, Using}

/** Responsible for handling JAR unpacking and handling the temporary build directory.
  */
object ProgramHandlingUtil {

  private val logger = LoggerFactory.getLogger(ProgramHandlingUtil.getClass)

  sealed class Entry(entry: Either[File, ZipEntry]) {

    def this(file: File) = this(Left(file))
    def this(entry: ZipEntry) = this(Right(entry))
    private def file: File            = entry.fold(identity, e => File(e.getName))
    def name: String                  = file.name
    def fullExtension: Option[String] = file.extension(includeAll = true)
    def extension: Option[String]     = file.extension
    def isDirectory: Boolean          = entry.fold(_.isDirectory, _.isDirectory)
    def maybeRegularFile(): Boolean   = entry.fold(_.isRegularFile, !_.isDirectory)
  }

  private def unfoldArchives[A](src: File, emitOrUnpack: File => Either[A, List[File]]): IterableOnce[A] = {
    emitOrUnpack(src) match {
      case Left(a)             => Seq(a)
      case Right(disposeFiles) => disposeFiles.flatMap(x => unfoldArchives(x, emitOrUnpack))
    }
  }
  private def extractClassesToTmp(
    src: File,
    tmpDir: File,
    isArchive: Entry => Boolean,
    isSource: Entry => Boolean
  ): IterableOnce[ClassFile] = {

    def shouldExtract(e: Entry) = e.maybeRegularFile() && (isArchive(e) || isSource(e))
    unfoldArchives(
      src,
      {
        case f if isSource(Entry(f)) =>
          Left(ClassFile(f))
        case f if f.isDirectory() =>
          val files = f.listRecursively.filterNot(_.isDirectory).toList
          Right(files)
        case f if isArchive(Entry(f)) =>
          val xTmp = File.newTemporaryDirectory("extract-archive-", parent = Some(tmpDir))
          val unzipDirs = Try(f.unzipTo(xTmp, e => shouldExtract(Entry(f)))) match {
            case Success(dir) => List(dir)
            case Failure(e) =>
              logger.warn(s"Failed to extract archive", e)
              List.empty
          }
          Right(unzipDirs)
        case _ =>
          Right(List.empty)
      }
    )
  }

  object ClassFile {
    def getPackagePathFromByteCode(fis: FileInputStream): Option[String] = {
      val cr = new ClassReader(fis)
      sealed class ClassNameVisitor extends ClassVisitor(Opcodes.ASM9) {
        var path: Option[String] = None
        override def visit(
          version: Int,
          access: Int,
          name: String,
          signature: String,
          superName: String,
          interfaces: Array[String]
        ): Unit = {
          path = Some(name)
        }
      }
      val rootVisitor = new ClassNameVisitor()
      cr.accept(rootVisitor, SKIP_CODE)
      rootVisitor.path
    }

    def getPackagePathFromByteCode(file: File): Option[String] =
      Try(file.fileInputStream.apply(getPackagePathFromByteCode))
        .recover {
          case e: Throwable => {
            logger.error(s"Error reading class file ${file.canonicalPath}", e)
            None
          }
        }
        .getOrElse(None)
  }
  sealed class ClassFile(val file: File, val packagePath: Option[String]) {
    def this(file: File) = this(file, ClassFile.getPackagePathFromByteCode(file))

    // TODO: Test that the path separator is always unix
    val components: Option[Array[String]] = packagePath.map(_.split("/"))

    val fullyQualifiedClassName: Option[String] = components.map(_.mkString("."))
    def moveToPackageLayoutIn(destDir: File): Option[ClassFile] =
      packagePath
        .map { path =>
          val destClass = destDir / s"${path}.class"
          if (destClass.exists()) {
            logger.warn(s"Overwriting class file: ${destClass.path.toAbsolutePath}")
          }
          destClass.parent.createDirectories();
          ClassFile(file.moveTo(destClass)(File.CopyOptions(overwrite = true)), packagePath)
        }
        .orElse {
          logger.warn(s"Missing package path for ${file.canonicalPath}. Failed to move to ${destDir.canonicalPath}")
          None
        }
  }
  def extractClassesInPackageLayout(
    src: File,
    destDir: File,
    isClass: Entry => Boolean,
    isArchive: Entry => Boolean
  ): List[ClassFile] =
    File
      .temporaryDirectory("extract-classes-")
      .apply(tmpDir =>
        extractClassesToTmp(src, tmpDir, isArchive, isClass).iterator
          .flatMap(_.moveToPackageLayoutIn(destDir))
          .toList
      )

}
