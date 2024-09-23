package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.LanguageFrontend.Java
import io.shiftleft.semanticcpg.typeinfo.dependencies.Dependency
import io.shiftleft.semanticcpg.typeinfo.{LanguageFrontend, PackageIdentifier, Version}

import java.io.File
import java.lang.ProcessBuilder
import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class GitSparseFetcher(repoUrl: String = "git@github.com:flandini/typeinfo.git", gitRef: String = "main") extends AutoCloseable {
  // TODO: constructor?
  private var firstDownload = true
  private val tmpDir = File.createTempFile("typeinfo-", "")
  if (!tmpDir.mkdir())
    throw new RuntimeException(s"Couldn't create temp dir: $tmpDir")
  private val tmpDirPath = tmpDir.toPath

  def getVersions(pid: PackageIdentifier): Try[List[String]] = {
    val depsDir = buildPackageDepsDirPath(pid)
    for {
      _ <- downloadPath(depsDir)
    } yield 
      File(buildFileSystemPath(depsDir))
      .listFiles()
      .map(_.getName)
      .map(stripCsvSuffix)
      .toList
  }

  def getDependencyInfo(pid: PackageIdentifier, version: Version): Try[List[Dependency]] = {
    val depsDir = buildPackageDepsDirPath(pid)
    val depFile = String.join(File.pathSeparator, depsDir, getDepsCsvFileName(version))
    for {
      _ <- downloadPath(depsDir)
      loadedDeps <- Dependency.fromFile(File(buildFileSystemPath(depFile)))
    } yield loadedDeps
  }

  def close(): Unit = {
    if (!tmpDir.delete())
      throw new RuntimeException("Couldn't delete temp dir")
  }
  
  // "data/" + "java/" + "java/io/File" -> "/tmp/typeinfo-1237asd/data/java/java/io/File"
  private def buildFileSystemPath(serverPath: String): String =
    tmpDirPath.resolve(serverPath).toString
    
  private def getDepsCsvFileName(version: Version): String = version.toFetcherStr + ".csv"

  // "java.io.File" -> "data/" + "java/" + "java/io/File" + "/deps"
  private def buildPackageDepsDirPath(pid: PackageIdentifier): String =
    String.join(File.pathSeparator, buildPackageDirPath(pid), "deps")

  // "java.io.File" -> "data/" + "java/" + "java/io/File"
  private def buildPackageDirPath(pid: PackageIdentifier): String =
    String.join(File.pathSeparator, "data", PackageIdentifier.langToString(pid.lang), pid.toFetcherStr)
    
  private def buildPackagePath(pid: PackageIdentifier, version: Version): String =
    String.join(File.pathSeparator, buildPackageDirPath(pid), version.toFetcherStr)

  private def initGitRepo(): Try[Unit] =
    for {
      exitCode <- sparseClone()
      _ <- checkExitCode(exitCode, "git sparse clone returned non-zero")
      exitCode <- setCone()
      _ <- checkExitCode(exitCode, "git sparse-checkout set --cone returned non-zero")
    } yield ()
    
  // First time download needs to `git sparse-checkout set /path/to/dir && git checkout $gitRef`; this checkout does
  // the download on 1st time.
  // Afterwards, `git sparse-checkout add /path/to/dir` adds to the sparse-checkout filter *and* downloads.
  private def downloadPath(pathStr: String): Try[Unit] =
    if (firstDownload) {
      for {
        exitCode <- setFirstPathFilter(pathStr)
        _ <- checkExitCode(exitCode, s"git sparse-checkout set $pathStr returned non-zero")
        exitCode <- doInitCheckout()
        _ <- checkExitCode(exitCode, s"git checkout main after sparse set of $pathStr returned non-zero")
      } yield ()
    } else {
      for {
        exitCode <- addPathFilterAndDownload(pathStr)
        _ <- checkExitCode(exitCode, s"git sparse-checkout add $pathStr returned non-zero")
      } yield ()
    }
  
  private def sparseClone(): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "clone", "--filter=blob:none", "--depth=1", "--no-checkout", repoUrl, tmpDirPath.toString).start().waitFor())
    
  private def setCone(): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "sparse-checkout", "set", "--cone").start().waitFor())
    
  private def setFirstPathFilter(pathStr: String): Try[Int] = {
    firstDownload = false
    Try(makeProcess(tmpDirPath, "git", "sparse-checkout", "set", pathStr).start().waitFor())
  }

  private def doInitCheckout(): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "checkout", gitRef).start().waitFor())

  private def addPathFilterAndDownload(pathStr: String): Try[Int] = {
    Try(makeProcess(tmpDirPath, "git", "sparse-checkout", "add", pathStr).start().waitFor())
  }
  
  private def stripCsvSuffix(fileName: String): String = 
    if (fileName.endsWith(".csv")) 
    then fileName.substring(0, fileName.length - ".csv".length) 
    else fileName
  
  private def checkExitCode(exitCode: Int, potentialErrMsg: => String): Try[Unit] =
    if (exitCode != 0) 
    then Failure(new RuntimeException(potentialErrMsg))
    else Success(())

  private def makeProcess(path: Path, command: String*): ProcessBuilder = {
    new ProcessBuilder().directory(path.toFile).command(command*).inheritIO()
  }
}
