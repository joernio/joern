package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM
import io.shiftleft.semanticcpg.typeinfo.dependencies.DirectDependency
import io.shiftleft.semanticcpg.typeinfo.loading.DependencyIonLoader
import io.shiftleft.semanticcpg.typeinfo.{LanguagePlatform, PackageIdentifier, Version}
import replpp.shaded.os.SubProcess.InputStream

import scala.jdk.CollectionConverters.*
import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.lang.ProcessBuilder
import java.util.Comparator
import scala.util.{Failure, Success, Try, Using}

class GitSparseFetcher(repoUrl: String = "git@github.com:flandini/typeinfo.git", gitRef: String = "main") extends Fetcher {
  private var firstDownload: Boolean = true
  private lazy val tmpDir: Path      = Files.createTempDirectory("typeinfo-")
//
//  def getVersions(pid: PackageIdentifier): Try[List[String]] = {
//    val depsDir = buildPackageDepsDirPath(pid)
//    for {
//      _ <- downloadPath(depsDir)
//    } yield Files
//      .list(buildFileSystemPath(depsDir))
//      .map(_.getFileName().toString)
//      .map(stripIonSuffix)
//      .iterator()
//      .asScala
//      .toList
//  }
//
//  def getDependencyInfo(pid: PackageIdentifier, version: Version): Try[List[DirectDependency]] = {
//    val depsDir  = buildPackageDepsDirPath(pid)
//    val depsFile = buildFileSystemPath(depsDir.resolve(getDepsIonFileName(version)))
//    for {
//      _           <- downloadPath(depsDir)
//      inputStream <- Try(Files.newInputStream(depsFile))
//      loadedDeps  <- DependencyIonLoader.parse(pid.platform, inputStream)
//    } yield loadedDeps
//  }

  override def close(): Unit = {
    Using.resource(Files.walk(tmpDir)) { fileStream =>
      // the iteration order in reverse visits files before their housing directory, so the directory is deleted after
      // it is empty.
      fileStream.sorted(Comparator.reverseOrder()).map(_.toFile).forEach(_.delete())
    }
  }

  /** "data/java/ion-java/" -> "/tmp/typeinfo-1237asd/data/java/ion-java/" */
  private def buildFileSystemPath(serverPath: Path): Path = tmpDir.resolve(serverPath)

  /** "v1.0.0" -> "v1.0.0.ion" */
  private def getDepsIonFileName(version: Version): String = version.toFetcherStr + ".ion"

  /** "ion-java" -> "data/" + "java/" + "ion-java/" + "deps/" */
  private def buildPackageDepsDirPath(pid: PackageIdentifier): Path = {
    buildPackageDirPath(pid).resolve("deps")
  }

  /** e.g., "ion-java" -> "data/" + "java/" + "ion-java/" */
  private def buildPackageDirPath(pid: PackageIdentifier): Path = {
    Paths.get("data", pid.platform.toString, pid.name)
  }

  /** e.g., "java.io.File", v1.0.0 -> "data/" + "java/" + "java */
  private def buildPackagePath(pid: PackageIdentifier, version: Version): Path = {
    buildPackageDirPath(pid).resolve(version.toFetcherStr)
  }
  
  /** 1. git sparse checkout uses directories paths, not file paths, so first, chop off the filename of each path
   * 2. download each directory path
   * 3. concat the temp directory + data/ + path (including file name) provided by fetcher to map each
   *    fetcher Path to a file system Path
   * 4. Open input streams to each file on the file system and return these as download results */
  override protected def downloadFiles(paths: List[Path]): List[DownloadResult] = {
    val dirPaths = paths.map(path => prependDataDir(path.getParent())).distinct
    downloadDirs(dirPaths)
    val inputStreams = paths
      .map(convertServerPathToFileSysPath)
      .map(Files.newInputStream(_))
    paths
      .zip(inputStreams)
      .map((path, inputStream) => DownloadResult(path, inputStream))
  }

  private def downloadDirs(dirPaths: List[Path]): Unit = {
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
  
  private def prependDataDir(path: Path): Path = {
    Paths.get("data").resolve(path)
  }
  
  /** The repo stores all data in the data/ dir */
  private def convertServerPathToFileSysPath(fetcherPath: Path): Path = {
    tmpDir.resolve(prependDataDir(fetcherPath))
  }

  private def sparseClone(): Int = {
      makeProcess(tmpDir, "git", "clone", "--filter=blob:none", "--depth=1", "--no-checkout", repoUrl, tmpDir.toString)
        .start()
        .waitFor()
  }

  private def setCone(): Int = {
    makeProcess(tmpDir, "git", "sparse-checkout", "set", "--cone").start().waitFor()
  }

  private def setInitPathFilter(paths: List[Path]): Int = {
    val args = Seq("git", "sparse-checkout", "set") ++ paths.toSeq.map(_.toString)
    makeProcess(tmpDir, args*).start().waitFor()
  }

  private def doInitCheckout(): Int = {
    makeProcess(tmpDir, "git", "checkout", gitRef).start().waitFor()
  }

  private def addPathFiltersAndDownload(paths: List[Path]): Int = {
    val args = Seq("git", "sparse-checkout", "add") ++ paths.toSeq.map(_.toString)
    makeProcess(tmpDir, args*).start().waitFor()
  }
  
  private def makeProcess(path: Path, command: String*): ProcessBuilder = {
    new ProcessBuilder().directory(path.toFile).command(command*).inheritIO()
  }
}
