import lmcoursier.internal.shaded.coursier.cache.Cache
import lmcoursier.internal.shaded.coursier.util.Artifact

import java.io.File
import java.nio.file.{Files, Path, Paths}

object DownloadHelper {
  private val LocalStorageDir = Paths.get(".local/source-urls")

  /** Downloads the remote file from the given url if either:
    *   - the localFile is not available,
    *   - or the url is different from the previously downloaded file
    *   - or we don't have the original url from the previously downloaded file
    *
    * We store the information about the previously downloaded urls and the localFile in `.local`
    */
  def ensureIsAvailable(url: String, localFile: File): Unit = {
    if (!localFile.exists() || Option(url) != previousUrlForLocalFile(localFile)) {
      val localPath = localFile.toPath
      Files.deleteIfExists(localPath)

      println(s"[INFO] downloading $url to $localFile")
      val file = downloadFile(url)
      sbt.IO.copyFile(file, localFile)

      // persist url in local storage
      val storageFile = storageInfoFileFor(localFile)
      Files.createDirectories(storageFile.getParent)
      Files.writeString(storageFile, url)
    }
  }

  private def downloadFile(url: String): File = {
    Cache.default
      .file(Artifact(url))
      .run
      .unsafeRun()(Cache.default.ec)
      // We re-throw if the download still fails after CoursierDownloadMaxRetry retries
      .fold(e => throw new java.io.IOException(e), identity)
  }

  private def relativePathToProjectRoot(path: Path): String =
    Paths
      .get("")
      .toAbsolutePath
      .normalize()
      .relativize(path.toAbsolutePath)
      .toString

  private def previousUrlForLocalFile(localFile: File): Option[String] = {
    Option(storageInfoFileFor(localFile))
      .filter(Files.exists(_))
      .map(Files.readString)
      .filter(_.nonEmpty)
  }

  private def storageInfoFileFor(localFile: File): Path =
    LocalStorageDir.resolve(relativePathToProjectRoot(localFile.toPath))
}
