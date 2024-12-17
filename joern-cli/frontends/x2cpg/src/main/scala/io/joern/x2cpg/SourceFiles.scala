package io.joern.x2cpg

import better.files.*
import better.files.File.VisitOptions
import org.slf4j.LoggerFactory

import java.io.FileNotFoundException
import java.nio.file.FileVisitor
import java.nio.file.FileVisitResult
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.Files
import scala.jdk.CollectionConverters.SetHasAsJava
import scala.util.matching.Regex

object SourceFiles {

  private val logger = LoggerFactory.getLogger(getClass)

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

    def files(): Array[File] = seenFiles.map(File(_)).toArray

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
    val filePathFile = File(filePath)
    if (!filePathFile.exists || !filePathFile.isReadable) {
      logger.debug(s"'$filePath' ignored (not readable or broken symlink)")
      return true
    }
    val isInIgnoredFiles = ignoredFiles.exists { ignorePath =>
      val ignorePathFile = File(ignorePath)
      ignorePathFile.exists &&
      (ignorePathFile.contains(filePathFile, strict = false) || ignorePathFile.isSameFileAs(filePathFile))
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
    * @return
    *   `true` if the file is accepted, i.e., does not match any of the ignore criteria, `false` otherwise.
    */
  def filterFile(
    file: String,
    inputPath: String,
    ignoredDefaultRegex: Option[Seq[Regex]] = None,
    ignoredFilesRegex: Option[Regex] = None,
    ignoredFilesPath: Option[Seq[String]] = None
  ): Boolean = !ignoredDefaultRegex.exists(isIgnoredByDefaultRegex(file, inputPath, _))
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

  private def hasSourceFileExtension(file: File, sourceFileExtensions: Set[String]): Boolean =
    sourceFileExtensions.exists(ext => file.pathAsString.endsWith(ext))

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
  )(implicit visitOptions: VisitOptions = VisitOptions.follow): List[String] = {
    val dir = File(inputPath)
    assertExists(dir)
    val visitor = new FailsafeFileVisitor(
      dir.pathAsString,
      sourceFileExtensions,
      ignoredDefaultRegex,
      ignoredFilesRegex,
      ignoredFilesPath
    )
    Files.walkFileTree(dir.path, visitOptions.toSet.asJava, Int.MaxValue, visitor)
    val matchingFiles = visitor.files().map(_.pathAsString)
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
  private def assertExists(file: File): Unit = {
    if (!file.exists) {
      logger.error(s"Source input path does not exist: ${file.pathAsString}")
      throw FileNotFoundException("Invalid source path provided!")
    }
    if (!file.isReadable) {
      logger.error(s"Source input path exists, but is not readable: ${file.pathAsString}")
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
        absolutePath.getFileName.toString
      } else {
        projectPath.relativize(absolutePath).toString
      }
    } else {
      path
    }
  }

}
