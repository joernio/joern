package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM
import io.shiftleft.semanticcpg.typeinfo.{LanguagePlatform, PackageIdentifier}
import io.shiftleft.semanticcpg.typeinfo.version.Version

import scala.jdk.CollectionConverters.*
import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.lang.ProcessBuilder
import java.util.Comparator
import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try, Using}

final class GitSparseFetcher(repoUrl: String = "git@github.com:flandini/typeinfo.git", gitRef: String = "main")
    extends Fetcher {
  private var firstDownload: Boolean = true
  private lazy val tmpDir: Path      = Files.createTempDirectory("typeinfo-")

  /** The repo stores all data in the data/ dir. Other top-level dirs and files are used for maintaining the type info
    * storage repo.
    */
  private class GitPath(serverPath: ServerPath) {
    val path: Path = Paths.get("data").resolve(serverPath.path)

    def getParent: GitPath = GitPath(ServerPath(serverPath.path.getParent))

    override def toString: String = path.toString

    override def equals(other: Any): Boolean = {
      other match
        case o: GitPath => path == o.path
        case _          => false
    }
  }

  private class FileSystemPath(gitPath: GitPath) {
    val path: Path = tmpDir.resolve(gitPath.path)

    def readBytes(): Array[Byte] = Files.newInputStream(path).readAllBytes()

    override def toString: String = path.toString
  }

  override def close(): Unit = {
    Using.resource(Files.walk(tmpDir)) { fileStream =>
      // the iteration order in reverse visits files before their housing directory, so the directory is deleted after
      // it is empty.
      fileStream.sorted(Comparator.reverseOrder()).map(_.toFile).forEach(_.delete())
    }
  }

  /**   1. git sparse checkout uses directories paths, not file paths, so first, chop off the filename of each path 2.
    *      download each directory path 3. concat the temp directory + data/ + path (including file name) provided by
    *      fetcher to map each fetcher Path to a file system Path 4. Open input streams to each file on the file system
    *      and return these as download results
    */
  override protected def downloadFiles(paths: List[ServerPath]): List[FetcherResult] = {
    val gitPaths    = paths.map(GitPath(_))
    val gitDirPaths = gitPaths.map(_.getParent).distinct

    downloadDirs(gitDirPaths)

    val fsPaths   = gitPaths.map(FileSystemPath(_))
    val downloads = fsPaths.map(_.readBytes())

    paths.zip(downloads).map((path, downloadedBytes) => FetcherResult(path, downloadedBytes))
  }

  private def downloadDirs(dirPaths: List[GitPath]): Unit = {
    if (firstDownload) {
      firstDownload = false
      if (sparseClone() != 0) {
        throw new Exception("git sparse clone returned non-zero")
      }
      if (setCone() != 0) {
        throw new Exception("git sparse-checkout set --cone returned non-zero")
      }
      if (setInitPathFilter(dirPaths) != 0) {
        throw new Exception(s"git sparse-checkout set ${dirPaths.mkString(" ")} returned non-zero")
      }
      if (doInitCheckout() != 0) {
        throw new Exception(s"git checkout main after sparse set of ${dirPaths.mkString(" ")} returned non-zero")
      }
    } else {
      if (addPathFiltersAndDownload(dirPaths) != 0) {
        throw new Exception(s"git sparse-checkout add ${dirPaths.mkString(" ")} returned non-zero")
      }
    }
  }

  private def sparseClone(): Int = {
    runProcess(tmpDir, "git", "clone", "--filter=blob:none", "--depth=1", "--no-checkout", repoUrl, tmpDir.toString)
  }

  private def setCone(): Int = {
    runProcess(tmpDir, "git", "sparse-checkout", "set", "--cone")
  }

  private def setInitPathFilter(paths: List[GitPath]): Int = {
    val args = Seq("git", "sparse-checkout", "set") ++ paths.toSeq.map(_.toString)
    runProcess(tmpDir, args*)
  }

  private def doInitCheckout(): Int = {
    runProcess(tmpDir, "git", "checkout", gitRef)
  }

  private def addPathFiltersAndDownload(paths: List[GitPath]): Int = {
    val args = Seq("git", "sparse-checkout", "add") ++ paths.toSeq.map(_.toString)
    runProcess(tmpDir, args*)
  }

  private def runProcess(path: Path, command: String*): Int = {
    val process = new ProcessBuilder()
      .directory(path.toFile)
      .command(command*)
      .inheritIO()
      .start()

    val finished = process.waitFor(30, TimeUnit.SECONDS)

    if (!finished)
      throw new RuntimeException(s"Running process $command timed out")
    else
      process.exitValue()
  }
}
