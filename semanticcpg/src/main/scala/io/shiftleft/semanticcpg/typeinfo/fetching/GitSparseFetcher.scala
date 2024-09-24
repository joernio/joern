package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.LanguageFrontend.Java
import io.shiftleft.semanticcpg.typeinfo.dependencies.Dependency
import io.shiftleft.semanticcpg.typeinfo.loading.DependencyIonLoader
import io.shiftleft.semanticcpg.typeinfo.{LanguageFrontend, PackageIdentifier, Version}

import scala.jdk.CollectionConverters._
import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.lang.ProcessBuilder
import scala.util.{Failure, Success, Try}

class GitSparseFetcher(repoUrl: String = "git@github.com:flandini/typeinfo.git", gitRef: String = "main") extends AutoCloseable {
  private var firstDownload: Boolean = true
  private lazy val tmpDir: Path = Files.createTempDirectory("typeinfo-")

  def getVersions(pid: PackageIdentifier): Try[List[String]] = {
    val depsDir = buildPackageDepsDirPath(pid)
    for {
      _ <- downloadPath(depsDir)
    } yield
      Files
      .list(buildFileSystemPath(depsDir))
      .map(_.getFileName().toString)
      .map(stripIonSuffix)
      .iterator()
      .asScala
      .toList
  }

  def getDependencyInfo(pid: PackageIdentifier, version: Version): Try[List[Dependency]] = {
    val depsDir = buildPackageDepsDirPath(pid)
    val depsFile = buildFileSystemPath(depsDir.resolve(getDepsIonFileName(version)))
    for {
      _ <- downloadPath(depsDir)
      inputStream <- Try(Files.newInputStream(depsFile))
      loadedDeps <- DependencyIonLoader.parse(pid.lang, inputStream)
    } yield loadedDeps
  }

  def close(): Unit = {
    if (!Files.deleteIfExists(tmpDir))
      throw new RuntimeException("Couldn't delete temp dir")
  }
  
  /** "data/java/ion-java/" -> "/tmp/typeinfo-1237asd/data/java/ion-java/" */
  private def buildFileSystemPath(serverPath: Path): Path = tmpDir.resolve(serverPath)
    
  /** "v1.0.0" -> "v1.0.0.ion" */
  private def getDepsIonFileName(version: Version): String = version.toFetcherStr + ".ion"

  /** "ion-java" -> "data/" + "java/" + "ion-java/" + "deps/" */
  private def buildPackageDepsDirPath(pid: PackageIdentifier): Path =
    buildPackageDirPath(pid).resolve("deps")

  /** e.g., "ion-java" -> "data/" + "java/" + "ion-java/" */
  private def buildPackageDirPath(pid: PackageIdentifier): Path = 
    Paths.get("data", pid.lang.toString, pid.name)
    
  /** e.g., "java.io.File", v1.0.0 -> "data/" + "java/" + "java */
  private def buildPackagePath(pid: PackageIdentifier, version: Version): Path =
    buildPackageDirPath(pid).resolve(version.toFetcherStr)
  
  // First time download needs to `git sparse-checkout set /path/to/dir && git checkout $gitRef`; this checkout does
  // the download on 1st time.
  // Afterwards, `git sparse-checkout add /path/to/dir` adds to the sparse-checkout filter *and* downloads.
  private def downloadPath(path: Path): Try[Unit] =
    if (firstDownload) {
      firstDownload = false
      for {
        exitCode <- sparseClone()
        _ <- checkExitCode(exitCode, "git sparse clone returned non-zero")
        exitCode <- setCone()
        _ <- checkExitCode(exitCode, "git sparse-checkout set --cone returned non-zero")
        exitCode <- setFirstPathFilter(path)
        _ <- checkExitCode(exitCode, s"git sparse-checkout set $path returned non-zero")
        exitCode <- doInitCheckout()
        _ <- checkExitCode(exitCode, s"git checkout main after sparse set of $path returned non-zero")
      } yield ()
    } else {
      for {
        exitCode <- addPathFilterAndDownload(path)
        _ <- checkExitCode(exitCode, s"git sparse-checkout add $path returned non-zero")
      } yield ()
    }
  
  private def sparseClone(): Try[Int] =
    Try(makeProcess(tmpDir, "git", "clone", "--filter=blob:none", "--depth=1", "--no-checkout", repoUrl, tmpDir.toString).start().waitFor())
    
  private def setCone(): Try[Int] =
    Try(makeProcess(tmpDir, "git", "sparse-checkout", "set", "--cone").start().waitFor())
    
  private def setFirstPathFilter(path: Path): Try[Int] =
    Try(makeProcess(tmpDir, "git", "sparse-checkout", "set", path.toString).start().waitFor())

  private def doInitCheckout(): Try[Int] =
    Try(makeProcess(tmpDir, "git", "checkout", gitRef).start().waitFor())

  private def addPathFilterAndDownload(path: Path): Try[Int] =
    Try(makeProcess(tmpDir, "git", "sparse-checkout", "add", path.toString).start().waitFor())
  
  private def stripIonSuffix(fileName: String): String = 
    if (fileName.endsWith(".ion")) 
    then fileName.substring(0, fileName.length - ".ion".length) 
    else fileName
  
  private def checkExitCode(exitCode: Int, potentialErrMsg: => String): Try[Unit] =
    if (exitCode != 0) 
    then Failure(new RuntimeException(potentialErrMsg))
    else Success(())

  private def makeProcess(path: Path, command: String*): ProcessBuilder =
    new ProcessBuilder().directory(path.toFile).command(command*).inheritIO()
}
