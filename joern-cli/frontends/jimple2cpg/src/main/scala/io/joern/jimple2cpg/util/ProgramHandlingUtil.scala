package io.joern.jimple2cpg.util

import io.joern.x2cpg.utils.FileUtil.*

import java.io.{BufferedInputStream, FileInputStream}
import java.nio.file.StandardCopyOption;

import better.files.*
import org.objectweb.asm.ClassReader.SKIP_CODE
import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import org.slf4j.LoggerFactory

import java.io.InputStream
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}
import scala.util.{Failure, Left, Success, Try, Using}
import scala.jdk.CollectionConverters._

/** Responsible for handling JAR unpacking and handling the temporary build directory.
  */
object ProgramHandlingUtil {

  private val logger = LoggerFactory.getLogger(ProgramHandlingUtil.getClass)

  /** Common properties of a File and ZipEntry, used to determine whether a file in a directory or an entry in an
    * archive is worth emitting/extracting
    */
  sealed class Entry(entry: Either[Path, ZipEntry], parentArchive: Option[ZipFile] = None) {

    def this(file: Path) = this(Left(file))
    def this(entry: ZipEntry, parentArchive: ZipFile) = this(Right(entry), Option(parentArchive))
    private def file: Path          = entry.fold(identity, e => Paths.get(e.getName))
    def name: String                = file.getFileName.toString
    def extension: Option[String]   = file.extension
    def isDirectory: Boolean        = entry.fold(Files.isDirectory(_), _.isDirectory)
    def maybeRegularFile(): Boolean = entry.fold(Files.isRegularFile(_), !_.isDirectory)

    /** Determines whether a zip entry is potentially malicious.
      * @return
      *   whether the entry is a ZipEntry and uses '..' in it's components
      */
    // Note that we consider either type of path separator as although the spec say that only
    // unix separators are to be used, zip files in the wild may vary.
    def isZipSlip: Boolean = entry.fold(_ => false, _.getName.split("[/\\\\]").contains(".."))

    def isZipFile: Boolean = entry match {
      case Left(file: Path) => isValidZipFile(file)
      case Right(zipEntry: ZipEntry) if !isZipSlip =>
        parentArchive.exists { f =>
          Using.resource(f.getInputStream(zipEntry)) { is =>
            File.temporaryFile("jimple2cpg-", ".zip").apply { f =>
              f.writeBytes(is.readAllBytes().iterator)
              isValidZipFile(f.path)
            }
          }
        }
      case _ => false
    }

    private def isValidZipFile(zis: ZipInputStream): Boolean = Try(zis.getNextEntry != null).getOrElse(false)

    /** Determines if the given file is a valid and uncorrupted zip file by reading through the whole archive until the
      * end.
      * @param f
      *   the file to read.
      * @return
      *   true if the file is a valid and uncorrupted zip file, false if otherwise.
      */
    private def isValidZipFile(f: Path): Boolean = {
      Using.Manager { use =>
        val fis = use(new FileInputStream(f.toString))
        val bis = use(new BufferedInputStream(fis))
        val zis = use(new ZipInputStream(bis))

        isValidZipFile(zis)
      } match {
        case Success(isValid) => isValid
        case Failure(_)       => false
      }
    }

    def isConfigFile: Boolean = {
      val configExt = Set(".xml", ".properties", ".yaml", ".yml", ".tf", ".tfvars", ".vm", ".jsp", ".conf", ".mf")

      def hasConfigExt(f: Path): Boolean = configExt.exists(f.extension.map(_.toLowerCase).contains(_))

      if (isDirectory) { false }
      else {
        entry match {
          case Left(file: Path)          => hasConfigExt(file)
          case Right(zipEntry: ZipEntry) => hasConfigExt(Paths.get(zipEntry.getName))
        }
      }
    }

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
    src: Path,
    emitOrUnpack: Path => Either[A, Map[Boolean, List[Path]]],
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
    * @param isConfigFile
    *   Where an entry is a config file
    * @param recurse
    *   Whether to unpack recursively
    * @param depth
    *   Maximum depth of recursion
    * @return
    *   The list of class files found, which may either be in [[src]] or in an extracted archive under [[tmpDir]]
    */
  private def extractClassesToTmp(
    src: Path,
    tmpDir: Path,
    isArchive: Entry => Boolean,
    isClass: Entry => Boolean,
    isConfigFile: Entry => Boolean,
    recurse: Boolean,
    depth: Int
  ): IterableOnce[EntryFile] = {

    // filter archive file unless recurse was enabled
    def shouldExtract(e: Entry) =
      !e.isZipSlip && e.maybeRegularFile() && ((isArchive(e) && recurse) || isClass(e) || isConfigFile(e))
    val subOfSrc = src.walk().filterNot(_ == src).filterNot(Files.isDirectory(_)).toList
    unfoldArchives(
      src,
      {
        case f if isClass(Entry(f)) =>
          Left(ClassFile(f))
        case f if isConfigFile(Entry(f)) =>
          Left(ConfigFile(f))
        case f if Files.isDirectory(f) =>
          val files = f.walk().filterNot(_ == src).filterNot(Files.isDirectory(_)).toList
          Right(Map(false -> files))
        case f if isArchive(Entry(f)) && (f == src || (Files.isDirectory(src) && subOfSrc.contains(f)) || recurse) =>
          val xTmp = Files.createTempDirectory(tmpDir, "extract-archive-")

          val unzipDirs =
            Using.resource(new ZipFile(f.absolutePathAsString)) { zipFile =>
              Try(f.unzipTo(xTmp, e => shouldExtract(Entry(e, zipFile)))) match {
                case Success(dir) => List(dir)
                case Failure(e) =>
                  logger.warn(s"Failed to extract archive", e)
                  List.empty
              }
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
    private def getPackagePathFromByteCode(file: Path): Option[String] =
      Using.resource(new FileInputStream(file.absolutePathAsString)) { fis =>
        Try(getPackagePathFromByteCode(fis))
          .recover { case e: Throwable =>
            logger.error(s"Error reading class file ${file.absolutePathAsString}", e)
            None
          }
          .getOrElse(None)
      }
  }

  sealed trait EntryFile {

    def file: Path

    def packagePath: Option[String]

    /** Copy the class file to its package path relative to [[destDir]]. This will overwrite a class file at the
      * destination if it exists.
      *
      * @param destDir
      *   The directory in which to place the class file
      * @return
      *   The class file at the destination if the package path could be retrieved from the its bytecode
      */
    def copyToPackageLayoutIn(destDir: Path): Option[EntryFile] = {
      packagePath
        .map { path =>
          val destFile = this match {
            case _: ClassFile  => destDir / s"$path.class"
            case _: ConfigFile =>
              // The temporary file naming prefix can be used as keys to extract the path parts
              val extractArchiveIdx = file.toString.indexOf("extract-archive")
              if (extractArchiveIdx != -1) {
                val relPath = file.getParent.toString
                  .substring(extractArchiveIdx)
                  .split(java.io.File.separatorChar)
                  .drop(1)
                  .mkString(java.io.File.separator)
                destDir / relPath / path
              } else {
                destDir / path
              }
          }
          if (Files.exists(destFile)) {
            logger.warn(s"Overwriting class file: ${destFile.toAbsolutePath}")
          }
          Files.createDirectories(destFile.getParent)
          file.copyTo(destFile, StandardCopyOption.REPLACE_EXISTING)
          ClassFile(destFile, packagePath)
        }
        .orElse {
          logger.warn(
            s"Missing package path for ${file.absolutePathAsString}. Failed to copy to ${destDir.absolutePathAsString}"
          )
          None
        }
    }

    override def toString: String = s"${getClass.getName}(${file.toString})"

  }

  private sealed class ConfigFile(val file: Path) extends EntryFile {

    def packagePath: Option[String] = Option(file.getFileName.toString)

  }

  sealed class ClassFile(val file: Path, val packagePath: Option[String]) extends EntryFile {

    def this(file: Path) = this(file, ClassFile.getPackagePathFromByteCode(file))

    private val components: Option[Array[String]] = packagePath.map(_.split("/"))

    val fullyQualifiedClassName: Option[String] = components.map(_.mkString("."))

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
    * @param isConfigFile
    *   Where an entry is a config file
    * @param recurse
    *   Whether to unpack recursively
    * @param depth
    *   Maximum depth of recursion
    * @return
    *   The copied class files in destDir
    */
  def extractClassesInPackageLayout(
    src: Path,
    destDir: Path,
    isClass: Entry => Boolean,
    isArchive: Entry => Boolean,
    isConfigFile: Entry => Boolean,
    recurse: Boolean,
    depth: Int
  ): List[ClassFile] =
    File
      .temporaryDirectory("extract-classes-")
      .apply(tmpDir =>
        extractClassesToTmp(src, tmpDir.path, isArchive, isClass, isConfigFile, recurse: Boolean, depth: Int).iterator
          .flatMap(_.copyToPackageLayoutIn(destDir))
          .collect { case x: ClassFile => x }
          .toList
      )

}
