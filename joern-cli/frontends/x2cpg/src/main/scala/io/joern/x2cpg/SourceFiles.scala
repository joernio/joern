package io.joern.x2cpg

import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.slf4j.LoggerFactory

import java.io.FileNotFoundException
import java.nio.file.{FileVisitOption, FileVisitResult, FileVisitor, Files, Path, Paths}
import java.nio.file.attribute.BasicFileAttributes
import scala.jdk.CollectionConverters.SetHasAsJava
import scala.util.matching.Regex
import scala.util.Try

object SourceFiles {

  private val logger = LoggerFactory.getLogger(getClass)

  // Supported formats (case-insensitive):
  //   - "123"
  //   - "123MB"
  //   - "2GB"
  //   - "1.5GB"
  //   - "0.25MB"
  private val FileSizePattern = "(?i)^([0-9]+(?:\\.[0-9]+)?)\\s*(MB|GB)?$".r

  private val MiB = 1024L * 1024L
  private val GiB = 1024L * 1024L * 1024L

  // Max. size for Array[Byte] in JVM is Integer.MAX_VALUE, but String can hold at most Integer.MAX_VALUE - 2 bytes
  private[x2cpg] val DefaultMaxFileSizeBytes = Integer.MAX_VALUE - 2L
  private[x2cpg] val DefaultMinFileSizeBytes = 1024L

  private[x2cpg] def parseMaxFileSize(value: String): Option[Long] = {
    val trimmed = Option(value).map(_.trim).getOrElse("")
    if (trimmed.isEmpty) return None
    trimmed match {
      case FileSizePattern(numberRaw, unitRaw) =>
        val factor = Option(unitRaw).map(_.toUpperCase) match {
          case Some("MB") => MiB.toDouble
          case Some("GB") => GiB.toDouble
          case None       => 1.0
          case _          => return None
        }

        Try {
          val num = numberRaw.toDouble
          if (num.isNaN || num.isInfinity) {
            None
          } else {
            val bytesDouble = num * factor
            // Round to nearest byte. Also guards against negative/zero
            // but never more than DefaultMaxFileSizeBytes because that's the maximum we can put into a String anyway.
            val bytes = Math.min(Math.round(bytesDouble), DefaultMaxFileSizeBytes)
            if (bytes < DefaultMinFileSizeBytes) Some(DefaultMinFileSizeBytes) else Some(bytes)
          }
        }.toOption.flatten
      case _ => None
    }
  }

  private def maxFileSizeFromEnv(): Long = {
    sys.env.get("MAX_FILE_SIZE") match {
      case None => DefaultMaxFileSizeBytes
      case Some(raw) =>
        parseMaxFileSize(raw).getOrElse {
          logger.info(
            s"Invalid MAX_FILE_SIZE='$raw'. Expected e.g., '512MB' or '1.5GB' (min. 1024B, max. 2GB). Falling back to $DefaultMaxFileSizeBytes bytes."
          )
          DefaultMaxFileSizeBytes
        }
    }
  }

  private val MaxFileSize: Long = maxFileSizeFromEnv()

  /** A failsafe implementation of a [[FileVisitor]] that continues iterating through files even if an [[IOException]]
    * occurs during traversal.
    *
    * This visitor determines during traversal whether a given file should be excluded based on several criteria, such
    * as matching default ignore patterns, specific file name patterns, or explicit file paths to ignore. It does not
    * descent into folders matching such ignore patterns.
    *
    * This class is useful in scenarios where file traversal must be resilient to errors, such as accessing files with
    * restricted permissions or encountering corrupted file entries.
    *
    * @param inputPath
    *   The root path from which the file traversal starts.
    * @param ignoredDefaultRegex
    *   Optional sequence of regular expressions to filter out default ignored file patterns.
    * @param ignoredFilesRegex
    *   Optional regular expression to filter out specific files based on their names.
    * @param ignoredFilesPath
    *   Optional sequence of file paths to exclude from traversal explicitly.
    */
  private final class FailsafeFileVisitor(
    inputPath: String,
    sourceFileExtensions: Set[String],
    ignoredDefaultRegex: Option[Seq[Regex]] = None,
    ignoredFilesRegex: Option[Regex] = None,
    ignoredFilesPath: Option[Seq[String]] = None
  ) extends FileVisitor[Path] {

    private val seenFiles = scala.collection.mutable.ArrayBuffer.empty[Path]

    def files(): Array[Path] = seenFiles.map(_.toAbsolutePath.normalize()).toArray

    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (filterFile(dir.toString, inputPath, ignoredDefaultRegex, ignoredFilesRegex, ignoredFilesPath)) {
        FileVisitResult.CONTINUE
      } else {
        FileVisitResult.SKIP_SUBTREE
      }
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (
        hasSourceFileExtension(file, sourceFileExtensions) &&
        filterFile(file.toString, inputPath, ignoredDefaultRegex, ignoredFilesRegex, ignoredFilesPath)
      ) { seenFiles.addOne(file) }
      FileVisitResult.CONTINUE
    }

    override def visitFileFailed(file: Path, exc: java.io.IOException): FileVisitResult = {
      exc match {
        case _: java.nio.file.FileSystemLoopException => logger.warn(s"Ignoring '$file' (cyclic symlink)")
        case other                                    => logger.warn(s"Ignoring '$file'", other)
      }
      FileVisitResult.CONTINUE
    }

    override def postVisitDirectory(dir: Path, exc: java.io.IOException): FileVisitResult = FileVisitResult.CONTINUE
  }

  private def isIgnoredByFileList(filePath: String, ignoredFiles: Seq[String]): Boolean = {
    val filePathFile = Paths.get(filePath)
    if (!Files.exists(filePathFile) || !Files.isReadable(filePathFile)) {
      logger.debug(s"'$filePath' ignored (not readable or broken symlink)")
      return true
    }
    val isInIgnoredFiles = ignoredFiles.exists { ignorePath =>
      val ignorePathFile = Paths.get(ignorePath)
      val containsIgnoreFilePath =
        Files.isDirectory(ignorePathFile) && filePathFile.startsWith(ignorePathFile) && ignorePathFile != filePathFile

      Files.exists(ignorePathFile) && (containsIgnoreFilePath || Files.isSameFile(ignorePathFile, filePathFile))
    }
    if (isInIgnoredFiles) {
      logger.debug(s"'$filePath' ignored (--exclude)")
      true
    } else {
      false
    }
  }

  private def isIgnoredByDefaultRegex(filePath: String, inputPath: String, ignoredDefaultRegex: Seq[Regex]): Boolean = {
    val relPath = toRelativePath(filePath, inputPath)
    if (ignoredDefaultRegex.exists(_.matches(relPath))) {
      logger.debug(s"'$relPath' ignored by default")
      true
    } else {
      false
    }
  }

  private def isIgnoredByRegex(filePath: String, inputPath: String, ignoredFilesRegex: Regex): Boolean = {
    val relPath               = toRelativePath(filePath, inputPath)
    val isInIgnoredFilesRegex = ignoredFilesRegex.matches(relPath)
    if (isInIgnoredFilesRegex) {
      logger.debug(s"'$relPath' ignored (--exclude-regex)")
      true
    } else {
      false
    }
  }

  private[x2cpg] def formatMaxFileSize(bytes: Long): String = {
    def formatRounded(value: Double, unit: String): String = {
      // Round to 1 decimal, but avoid trailing .0
      val rounded = Math.round(value * 10.0) / 10.0
      if (rounded.isWhole) s"${rounded.toLong}$unit" else s"$rounded$unit"
    }

    if (bytes >= GiB) {
      formatRounded(bytes.toDouble / GiB.toDouble, "GB")
    } else if (bytes >= MiB) {
      formatRounded(bytes.toDouble / MiB.toDouble, "MB")
    } else {
      s"${bytes}B"
    }
  }

  /** Returns true if `file` is a regular file whose size exceeds `maxFileSize`.
    *
    * Directories and non-regular files (e.g., devices, sockets) are treated as not too large. If the size cannot be
    * determined (e.g., due to an `IOException`), this function returns `false`.
    *
    * @param file
    *   Path to the file to check.
    * @param maxFileSize
    *   Maximum allowed size in bytes.
    */
  private def isTooLarge(file: String, maxFileSize: Long): Boolean = {
    val path             = Paths.get(file)
    val maxFileSizeBytes = Math.max(DefaultMinFileSizeBytes, Math.min(maxFileSize, DefaultMaxFileSizeBytes))
    if (Files.isDirectory(path) || !Files.isRegularFile(path)) return false
    Try {
      val size     = Files.size(path)
      val tooLarge = size > maxFileSizeBytes
      if (tooLarge) {
        logger.warn(s"'$file' ignored (too large: ${size}B > ${formatMaxFileSize(maxFileSizeBytes)})")
      }
      tooLarge
    }.getOrElse(false)
  }

  /** Filters a file based on the provided ignore rules.
    *
    * This method determines whether a given file should be excluded from processing based on several criteria, such as
    * matching default ignore patterns, specific file name patterns, or explicit file paths to ignore.
    *
    * @param file
    *   The file name or path to evaluate.
    * @param inputPath
    *   The root input path for the file traversal.
    * @param ignoredDefaultRegex
    *   Optional sequence of regular expressions defining default file patterns to ignore.
    * @param ignoredFilesRegex
    *   Optional regular expression defining specific file name patterns to ignore.
    * @param ignoredFilesPath
    *   Optional sequence of file paths to explicitly exclude.
    * @param maxFileSize
    *   * Optional maximum accepted size in bytes. Defaults to [[SourceFiles.MaxFileSize]].
    * @return
    *   `true` if the file is accepted, i.e., does not match any of the ignore criteria, `false` otherwise.
    */
  def filterFile(
    file: String,
    inputPath: String,
    ignoredDefaultRegex: Option[Seq[Regex]] = None,
    ignoredFilesRegex: Option[Regex] = None,
    ignoredFilesPath: Option[Seq[String]] = None,
    maxFileSize: Long = MaxFileSize
  ): Boolean =
    !isTooLarge(file, maxFileSize)
      && !ignoredDefaultRegex.exists(isIgnoredByDefaultRegex(file, inputPath, _))
      && !ignoredFilesRegex.exists(isIgnoredByRegex(file, inputPath, _))
      && !ignoredFilesPath.exists(isIgnoredByFileList(file, _))

  /** Filters a list of files based on the provided ignore rules.
    *
    * This method applies [[filterFile]] to each file in the input list, returning only those files that do not match
    * any of the ignore criteria.
    *
    * @param files
    *   The list of file names or paths to evaluate.
    * @param inputPath
    *   The root input path for the file traversal.
    * @param ignoredDefaultRegex
    *   Optional sequence of regular expressions defining default file patterns to ignore.
    * @param ignoredFilesRegex
    *   Optional regular expression defining specific file name patterns to ignore.
    * @param ignoredFilesPath
    *   Optional sequence of file paths to explicitly exclude.
    * @return
    *   A filtered list of files that do not match the ignore criteria.
    */
  def filterFiles(
    files: List[String],
    inputPath: String,
    ignoredDefaultRegex: Option[Seq[Regex]] = None,
    ignoredFilesRegex: Option[Regex] = None,
    ignoredFilesPath: Option[Seq[String]] = None
  ): List[String] = files.filter(filterFile(_, inputPath, ignoredDefaultRegex, ignoredFilesRegex, ignoredFilesPath))

  private def hasSourceFileExtension(file: Path, sourceFileExtensions: Set[String]): Boolean =
    sourceFileExtensions.exists(ext => file.toString.endsWith(ext))

  /** Determines a sorted list of file paths in a directory that match the specified criteria.
    *
    * @param inputPath
    *   The root directory to search for files.
    * @param sourceFileExtensions
    *   A set of file extensions to include in the search.
    * @param ignoredDefaultRegex
    *   An optional sequence of regular expressions for default files to ignore.
    * @param ignoredFilesRegex
    *   An optional regular expression for additional files to ignore.
    * @param ignoredFilesPath
    *   An optional sequence of specific file paths to ignore.
    * @param visitOptions
    *   Implicit parameter defining the options for visiting the file tree. Defaults to `VisitOptions.follow`, which
    *   follows symbolic links.
    * @return
    *   A sorted `List[String]` of file paths matching the criteria.
    *
    * This function traverses the file tree starting at the given `inputPath` and collects file paths that:
    *   - Have extensions specified in `sourceFileExtensions`.
    *   - Are not ignored based on `ignoredDefaultRegex`, `ignoredFilesRegex`, or `ignoredFilesPath`.
    *
    * It uses a custom `FailsafeFileVisitor` to handle the filtering logic and `Files.walkFileTree` to perform the
    * traversal.
    *
    * Example usage:
    * {{{
    * val files = determine(
    *   inputPath = "/path/to/dir",
    *   sourceFileExtensions = Set(".scala", ".java"),
    *   ignoredDefaultRegex = Some(Seq(".*\\.tmp".r)),
    *   ignoredFilesRegex = Some(".*_backup\\.scala".r),
    *   ignoredFilesPath = Some(Seq("/path/to/dir/ignore_me.scala"))
    * )
    * println(files)
    * }}}
    * @throws java.io.FileNotFoundException
    *   if the `inputPath` does not exist or is not readable.
    * @see
    *   [[FailsafeFileVisitor]] for details on the visitor used to process files.
    */
  def determine(
    inputPath: String,
    sourceFileExtensions: Set[String],
    ignoredDefaultRegex: Option[Seq[Regex]] = None,
    ignoredFilesRegex: Option[Regex] = None,
    ignoredFilesPath: Option[Seq[String]] = None
  )(implicit visitOptions: Seq[FileVisitOption] = Seq(FileVisitOption.FOLLOW_LINKS)): List[String] = {
    val dir = Paths.get(inputPath)
    assertExists(dir)
    val visitor = new FailsafeFileVisitor(
      dir.toString,
      sourceFileExtensions,
      ignoredDefaultRegex,
      ignoredFilesRegex,
      ignoredFilesPath
    )
    Files.walkFileTree(dir, visitOptions.toSet.asJava, Int.MaxValue, visitor)
    val matchingFiles = visitor.files().map(_.toString)
    matchingFiles.toList.sorted
  }

  /** Asserts that a given file exists and is readable.
    *
    * This method validates the existence and readability of the specified file. If the file does not exist or is not
    * readable, it logs an error and throws a [[FileNotFoundException]].
    *
    * @param file
    *   The file to validate.
    * @throws FileNotFoundException
    *   if the file does not exist or is not readable.
    */
  private def assertExists(file: Path): Unit = {
    if (!Files.exists(file)) {
      logger.error(s"Source input path does not exist: ${file.toString}")
      throw FileNotFoundException("Invalid source path provided!")
    }
    if (!Files.isReadable(file)) {
      logger.error(s"Source input path exists, but is not readable: ${file.toString}")
      throw FileNotFoundException("Invalid source path provided!")
    }
  }

  /** Constructs an absolute path against rootPath. If the given path is already absolute this path is returned
    * unaltered. Otherwise, "rootPath / path" is returned.
    */
  def toAbsolutePath(path: String, rootPath: String): String = {
    val absolutePath = Paths.get(path) match {
      case p if p.isAbsolute            => p
      case _ if rootPath.endsWith(path) => Paths.get(rootPath)
      case p                            => Paths.get(rootPath, p.toString)
    }
    absolutePath.normalize().toString
  }

  /** Constructs a relative path against rootPath. If the given path is not inside rootPath, path is returned unaltered.
    * Otherwise, the path relative to rootPath is returned.
    */
  def toRelativePath(path: String, rootPath: String): String = {
    if (path.startsWith(rootPath)) {
      val absolutePath = Paths.get(path).toAbsolutePath
      val projectPath  = Paths.get(rootPath).toAbsolutePath
      if (absolutePath.compareTo(projectPath) == 0) {
        absolutePath.fileName
      } else {
        projectPath.relativize(absolutePath).toString
      }
    } else {
      path
    }
  }

}
