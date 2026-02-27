import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.*
import UrlRetry.*

object DownloadHelper {
  private val LocalStorageDir = Paths.get(".local/source-urls")

  /** Downloads the remote file from the given url if either
    *   - the localFile is not available,
    *   - or the url is different from the previously downloaded file
    *   - or we don't have the original url from the previously downloaded file We store the information about the
    *     previously downloaded urls and the localFile in `.local`
    */
  def ensureIsAvailable(url: String, localFile: File): Unit = {
    if (!localFile.exists() || Option(url) != previousUrlForLocalFile(localFile)) {
      val localPath = localFile.toPath
      Files.deleteIfExists(localPath)

      println(s"[INFO] downloading $url to $localFile")
      val urlFromString = new URI(url).toURL
      withTransientHttpRetries(maxRetries = 5, baseInterval = 500.millis, backoffFactor = 2.0) {
        val conn = openAndCheck(urlFromString)
        try {
          sbt.io.Using.bufferedInputStream(conn.getInputStream) { inputStream =>
            sbt.IO.transfer(inputStream, localFile)
          }
        } finally conn.disconnect()
      }

      // persist url in local storage
      val storageFile = storageInfoFileFor(localFile)
      Files.createDirectories(storageFile.getParent)
      Files.writeString(storageFile, url)
    }
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
