package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.LanguageFrontend.Java
import io.shiftleft.semanticcpg.typeinfo.{LanguageFrontend, PackageIdentifier, Version}

import java.io.File
import java.lang.ProcessBuilder
import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class GitSparseFetcher(repoUrl: String = "git@github.com:flandini/typeinfo.git",
                       gitRef: String = "main") 
  extends AutoCloseable {
  
  // TODO: constructor?
  private val tmpDir = File.createTempFile("typeinfo-", "")
  if (!tmpDir.mkdir())
    throw new RuntimeException("Couldn't create temp dir")
  private val tmpDirPath = tmpDir.toPath

  def close(): Unit = {
    if (!tmpDir.delete())
      throw new RuntimeException("Couldn't delete temp dir")
  }

  def getVersions(pid: PackageIdentifier): Either[FetchingError, List[String]] = {
    val depsDir: String = buildPathStr(pid, depsDir = true)
    downloadPath(depsDir)
  }
  
  private def langToStr(lang: LanguageFrontend): String =
    lang match
      case Java => "java"
      case _ => throw new Exception(s"type info fetching not implemented for lang $lang")

  private def buildPathStr(pid: PackageIdentifier, depsDir: Boolean = false): String = {
    val sb = StringBuilder()
    sb.append("data")
    sb.append(File.pathSeparator)
    sb.append(langToStr(pid.lang))
    sb.append(File.pathSeparator)
    sb.append(pid.toFetcherStr)
    if (depsDir) {
      sb.append(File.pathSeparator)
      sb.append("deps")
    }
    sb.toString()
  }

  private def initGitRepo(): Try[Unit] =
    for {
      exitCode <- sparseClone()
      _ <- checkExitCode(exitCode, "git sparse clone returned non-zero")
      exitCode <- setCone()
      _ <- checkExitCode(exitCode, "git sparse-checkout set --cone returned non-zero")
    } yield Success(())
    
  private def downloadPath(pathStr: String): Try[Unit] =
    for {
      exitCode <- setPathFilter(pathStr)
      _ <- checkExitCode(exitCode, s"git sparse-checkout set $pathStr returned non-zero")
      exitCode <- doDownload()
      _ <- checkExitCode(exitCode, s"git checkout main after sparse set of $pathStr returned non-zero")
    } yield Success(())
  
  private def sparseClone(): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "clone", "--filter=blob:none", "--depth=1", "--no-checkout", repoUrl, tmpDirPath.toString).start().waitFor())
    
  private def setCone(): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "sparse-checkout", "set", "--cone").start().waitFor())
    
  private def setPathFilter(pathStr: String): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "sparse-checkout", "set", pathStr).start().waitFor())
    
  private def doDownload(): Try[Int] =
    Try(makeProcess(tmpDirPath, "git", "checkout", gitRef).start().waitFor())
  
  private def checkExitCode(exitCode: Int, potentialErrMsg: => String): Try[Unit] =
    if (exitCode != 0) 
    then Failure(new RuntimeException(potentialErrMsg))
    else Success(())

  private def makeProcess(path: Path, command: String*): ProcessBuilder = {
    new ProcessBuilder().directory(path.toFile).command(command*).inheritIO()
  }
}
