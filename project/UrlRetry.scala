import java.io.File
import java.net.{HttpURLConnection, URL}
import scala.concurrent.duration.*

object UrlRetry {

  final case class HttpStatusException(status: Int, url: URL, msg: String)
      extends RuntimeException(s"HTTP $status for $url: $msg")

  /** Retries `block` up to `maxRetries` times on any exception.
    * Waits `baseInterval * backoffFactor^(retry-1)` before each retry (exponential backoff).
    */
  private def withRetries[T](maxRetries: Int, baseInterval: FiniteDuration, backoffFactor: Double)(block: => T): T = {
    require(maxRetries >= 1, "maxRetries must be >= 1")
    require(baseInterval.toMillis >= 0, "baseInterval must be >= 0")
    require(backoffFactor >= 1.0, "backoffFactor must be >= 1.0")

    def loop(attempt: Int, lastError: Option[Throwable]): T = {
      lastError.foreach { e =>
        println(s"[WARN] Attempt $attempt/$maxRetries failed with: ${e.getMessage}")
        val delayMs = (baseInterval.toMillis.toDouble * math.pow(backoffFactor, attempt - 2)).toLong
        if (delayMs > 0) Thread.sleep(delayMs)
      }
      try block
      catch {
        case e: Throwable =>
          if (attempt >= maxRetries) throw e
          loop(attempt + 1, Some(e))
      }
    }

    loop(attempt = 1, lastError = None)
  }

  /** Opens a connection to `url` and returns it if the response is 2xx, otherwise throws [[HttpStatusException]]. */
  private def openAndCheck(url: URL): HttpURLConnection = {
    val conn   = url.openConnection().asInstanceOf[HttpURLConnection]
    val status = conn.getResponseCode
    if (status >= 200 && status <= 299) conn
    else {
      val msg = Option(conn.getResponseMessage).getOrElse("no message")
      conn.disconnect()
      throw HttpStatusException(status, url, msg)
    }
  }

  /** Downloads `url` to `localFile`, retrying on any error. */
  def downloadWithRetries(
    url: URL,
    localFile: File,
    maxRetries: Int = 5,
    baseInterval: FiniteDuration = 5.seconds,
    backoffFactor: Double = 3.0
  ): Unit = {
    withRetries(maxRetries, baseInterval, backoffFactor) {
      val conn = openAndCheck(url)
      try sbt.io.Using.bufferedInputStream(conn.getInputStream)(sbt.IO.transfer(_, localFile))
      finally conn.disconnect()
    }
  }
}
