package io.joern.jimple2cpg.util

import better.files.*
import org.objectweb.asm.ClassReader.SKIP_CODE
import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import org.slf4j.LoggerFactory

import java.io.InputStream
import java.util.zip.ZipEntry
import scala.util.{Failure, Left, Success, Try, boundary}

/** Responsible for handling JAR unpacking and handling the temporary build directory.
  */
object ProgramHandlingUtil {

  private val logger = LoggerFactory.getLogger(ProgramHandlingUtil.getClass)

  /** Common properties of a File and ZipEntry, used to determine whether a file in a directory or an entry in an
    * archive is worth emitting/extracting
    */
  sealed class Entry(entry: Either[File, ZipEntry]) {

    def this(file: File) = this(Left(file))
    def this(entry: ZipEntry) = this(Right(entry))
    private def file: File          = entry.fold(identity, e => File(e.getName))
    def name: String                = file.name
    def extension: Option[String]   = file.extension
    def isDirectory: Boolean        = entry.fold(_.isDirectory, _.isDirectory)
    def maybeRegularFile(): Boolean = entry.fold(_.isRegularFile, !_.isDirectory)

    /** Determines whether a zip entry is potentially malicious.
      * @return
      *   whether the entry is a ZipEntry and uses '..' in it's components
      */
    // Note that we consider either type of path separator as although the spec say that only
    // unix separators are to be used, zip files in the wild may vary.
    def isZipSlip: Boolean = entry.fold(_ => false, _.getName.split("[/\\\\]").contains(".."))
  }

  /** Process files that may lead to more files to process or to emit a resulting value of [[A]]
    *
    * @param src
    *   The file/directory to traverse
    * @param emitOrUnpack
    *   A function that takes a file and either emits a value or returns more files to traverse. The key of Right was
    *   used to identify whether the file was unfold from an archive
    * @param maxDepth
    *   The max recursion depth of unpacking an archive (e.g. jars inside jars)
    * @tparam A
    *   The type of emitted values
    * @return
    *   The emitted values
    */
  private def unfoldArchives[A](
    src: File,
    emitOrUnpack: File => Either[A, Map[Boolean, List[File]]],
    maxDepth: Int
  ): IterableOnce[A] = {
    if (maxDepth < -1)
      logger.warn("Maximum recursion depth reached.")
      Seq()
    else
      emitOrUnpack(src) match {
        case Left(a) => Seq(a)
        case Right(disposeFiles) =>
          disposeFiles.flatMap(x =>
            x._2.flatMap(f =>
              if (x._1)
                unfoldArchives(f, emitOrUnpack, maxDepth - 1)
              else
                unfoldArchives(f, emitOrUnpack, maxDepth)
            )
          )
      }
  }

  /** Find <pre>.class</pre> files, including those inside archives.
    *
    * @param src
    *   The file/directory to search.
    * @param tmpDir
    *   A temporary directory for extracted archives
    * @param isArchive
    *   Whether an entry is an archive to extract
    * @param isClass
    *   Whether an entry is a class file
    * @param recurse
    *   Whether to unpack recursively
    * @param depth
    *   Maximum depth of recursion
    * @return
    *   The list of class files found, which may either be in [[src]] or in an extracted archive under [[tmpDir]]
    */
  private def extractClassesToTmp(
    src: File,
    tmpDir: File,
    isArchive: Entry => Boolean,
    isClass: Entry => Boolean,
    recurse: Boolean,
    depth: Int
  ): IterableOnce[ClassFile] = {

    // filter archive file unless recurse was enabled
    def shouldExtract(e: Entry) = !e.isZipSlip && e.maybeRegularFile() && ((isArchive(e) && recurse) || isClass(e))
    val subOfSrc                = src.listRecursively.filterNot(_.isDirectory).toList
    unfoldArchives(
      src,
      {
        case f if isClass(Entry(f)) =>
          Left(ClassFile(f))
        case f if f.isDirectory() =>
          val files = f.listRecursively.filterNot(_.isDirectory).toList
          Right(Map(false -> files))
        case f if isArchive(Entry(f)) && (f == src || (src.isDirectory() && subOfSrc.contains(f)) || recurse) =>
          val xTmp = File.newTemporaryDirectory("extract-archive-", parent = Some(tmpDir))
          val unzipDirs = Try(f.unzipTo(xTmp, e => shouldExtract(Entry(e)))) match {
            case Success(dir) => List(dir)
            case Failure(e) =>
              logger.warn(s"Failed to extract archive", e)
              List.empty
          }
          // This can always be true, since the archive file was filtered by shouldExtract if recurse options not enabled.
          Right(Map(true -> unzipDirs))
        case _ =>
          Right(Map(false -> List.empty))
      },
      depth
    )
  }

  object ClassFile {
    private def getPackagePathFromByteCode(is: InputStream): Option[String] = {
      val cr = new ClassReader(is)
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

    /** Attempt to retrieve the package path from JVM bytecode.
      *
      * @param file
      *   The class file
      * @return
      *   The package path if successfully retrieved
      */
    private def getPackagePathFromByteCode(file: File): Option[String] =
      Try(file.fileInputStream.apply(getPackagePathFromByteCode))
        .recover { case e: Throwable =>
          logger.error(s"Error reading class file ${file.canonicalPath}", e)
          None
        }
        .getOrElse(None)
  }
  sealed class ClassFile(val file: File, val packagePath: Option[String]) {
    def this(file: File) = this(file, ClassFile.getPackagePathFromByteCode(file))

    private val components: Option[Array[String]] = packagePath.map(_.split("/"))

    val fullyQualifiedClassName: Option[String] = components.map(_.mkString("."))

    /** Copy the class file to its package path relative to [[destDir]]. This will overwrite a class file at the
      * destination if it exists.
      * @param destDir
      *   The directory in which to place the class file
      * @return
      *   The class file at the destination if the package path could be retrieved from the its bytecode
      */
    def copyToPackageLayoutIn(destDir: File): Option[ClassFile] =
      packagePath
        .map { path =>
          val destClass = destDir / s"$path.class"
          if (destClass.exists()) {
            logger.warn(s"Overwriting class file: ${destClass.path.toAbsolutePath}")
          }
          destClass.parent.createDirectories()
          ClassFile(file.copyTo(destClass)(File.CopyOptions(overwrite = true)), packagePath)
        }
        .orElse {
          logger.warn(s"Missing package path for ${file.canonicalPath}. Failed to copy to ${destDir.canonicalPath}")
          None
        }
  }

  /** Find <pre>.class</pre> files, including those inside archives and copy them to their package path location
    * relative to [[destDir]]
    *
    * @param src
    *   The file/directory to search.
    * @param destDir
    *   The directory in which to place the class files
    * @param isArchive
    *   Whether an entry is an archive to extract
    * @param isClass
    *   Whether an entry is a class file
    * @param recurse
    *   Whether to unpack recursively
    * @param depth
    *   Maximum depth of recursion
    * @return
    *   The copied class files in destDir
    */
  def extractClassesInPackageLayout(
    src: File,
    destDir: File,
    isClass: Entry => Boolean,
    isArchive: Entry => Boolean,
    recurse: Boolean,
    depth: Int
  ): List[ClassFile] =
    File
      .temporaryDirectory("extract-classes-")
      .apply(tmpDir =>
        extractClassesToTmp(src, tmpDir, isArchive, isClass, recurse: Boolean, depth: Int).iterator
          .flatMap(_.copyToPackageLayoutIn(destDir))
          .toList
      )

}
