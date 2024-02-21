import sbt.IO

import java.io.File
import java.net.{URI, URLEncoder}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Paths}
import java.time.Instant
import java.time.temporal.ChronoUnit

object SimpleCache {
  val LocalCacheDir = ".local"

  /** download given url if not yet present in cache, then add to cache warning: only use for immutable artifacts, i.e.
    * do not use for snapshot artifacts that may change
    */
  def downloadMaybe(url: String): File = {
    wipeOldCachedFiles()

    val localFile = encodeFile(url)
    if (!localFile.exists) {
      println(s"downloading $url")
      sbt.io.Using.urlInputStream(new URI(url).toURL) { inputStream =>
        IO.transfer(inputStream, localFile)
      }
    }
    localFile
  }

  /** In order for the cache not to grow up too much in size, we want to clear old entries.
   * This cache is so simple though that we have no idea when a file was last recently used, so we bite the bullet
   * and simply delete all files older than 30 days.
   * (In theory one could change the api to allow for that, but that'd make the SimpleCache api less 'simple')
   * */
  private def wipeOldCachedFiles(): Unit = {
    val root = Paths.get(LocalCacheDir)
    if (Files.exists(root)) {
      Files.walk(root).filter(Files.isRegularFile(_)).forEach { path =>
        val creationTime = Files.readAttributes(path, classOf[BasicFileAttributes]).creationTime().toInstant
        val cutoffDate = Instant.now().minus(30, ChronoUnit.DAYS)
        if (creationTime.compareTo(cutoffDate) == -1) {
          println(s"[DEBUG] deleting old file from $LocalCacheDir (simply based on creation time...)")
          Files.delete(path)
        }
      }
    }
  }

  def encodeFile(url: String): File = {
    val urlEncoded = URLEncoder.encode(url, "UTF-8")
    new File(s"$LocalCacheDir/$urlEncoded")
  }
}
