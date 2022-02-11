import sbt.{IO, URL}
import java.io.File
import java.net.URLEncoder

object SimpleCache {
  val LocalCacheDir = ".local"

  /** download given url if not yet present in cache, then add to cache warning: only use for immutable artifacts, i.e.
    * do not use for snapshot artifacts that may change
    */
  def downloadMaybe(url: String): File = {
    val urlEncoded = URLEncoder.encode(url, "UTF-8")
    val localFile  = new File(s"$LocalCacheDir/$urlEncoded")

    if (!localFile.exists) {
      println(s"downloading $url")
      sbt.io.Using.urlInputStream(new URL(url)) { inputStream =>
        IO.transfer(inputStream, localFile)
      }
    }

    localFile
  }

}
