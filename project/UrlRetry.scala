import java.io.{File, IOException}
import java.net.{HttpURLConnection, URL}
import scala.concurrent.duration.*

object UrlRetry {

  /** Set of transient HTTP status codes that are considered safe to retry:
    *   - 408: Request Timeout – server timed out waiting for the request
    *   - 425: Too Early – server unwilling to process a possibly replayed request
    *   - 429: Too Many Requests – client is being rate-limited
    *   - 500: Internal Server Error – generic server-side failure
    *   - 502: Bad Gateway – upstream server returned an invalid response
    *   - 503: Service Unavailable – server temporarily overloaded or down
    *   - 504: Gateway Timeout – upstream server didn't respond in time
    */
  private val TransientStatusCodes: Set[Int] =
    Set(408, 425, 429, 500, 502, 503, 504)

  /** Base type for HTTP-related failures. */
  sealed abstract class HttpException(message: String) extends RuntimeException(message)

  /** Thrown when we got an HTTP response with a status code. */
  final case class HttpStatusException(status: Int, url: URL, msg: String)
      extends HttpException(s"HTTP $status for $url: $msg")

  /** Marker exception used to trigger retries (for transient HTTP statuses). */
  final case class TransientHttpException(status: Int, url: URL, msg: String)
      extends HttpException(s"Transient HTTP $status for $url: $msg")

  /** Retries a block that uses URL.openConnection (or any other network I/O) up to `maxRetries`.
    *
    *   - Retries for transient HTTP status codes (defaults in TransientStatusCodes) and IOExceptions.
    *   - Retry interval and backoff are configurable.
    *   - Backoff is applied multiplicatively.
    *
    * Notes:
    *   - The `block` should perform the full request and either: (a) throw on non-2xx (via `openAndCheck` below), or
    *     (b) return some value you define after inspecting the response code.
    */
  def withTransientHttpRetries[T](maxRetries: Int, baseInterval: FiniteDuration, backoffFactor: Double)(
    block: => T
  ): T = {
    require(maxRetries >= 1, "maxRetries must be >= 1")
    require(baseInterval.toMillis >= 0, "baseInterval must be >= 0")
    require(backoffFactor >= 1.0, "backoffFactor must be >= 1.0")

    def sleepForAttempt(attempt: Int): Unit = {
      // attempt is 1-based; attempt 1 means "first try" => no sleep before it
      if (attempt > 1) {
        val multiplier = math.pow(backoffFactor, attempt - 2) // before 2nd try => pow(...,0)
        val delayMs    = (baseInterval.toMillis.toDouble * multiplier).toLong
        if (delayMs > 0) Thread.sleep(delayMs)
      }
    }

    def loop(attempt: Int, lastError: Throwable): T = {
      if (attempt > 1) println(s"[WARN] Attempt $attempt/$maxRetries failed with: ${lastError.getMessage}")
      sleepForAttempt(attempt)
      try {
        block
      } catch {
        case e: TransientHttpException if TransientStatusCodes.contains(e.status) =>
          if (attempt >= maxRetries) throw e
          loop(attempt + 1, e)
        case e: IOException =>
          if (attempt >= maxRetries) throw e
          loop(attempt + 1, e)
        // If it's an HTTP exception but not transient -> do not retry
        case e: HttpException =>
          throw e
        // Non-HTTP failures are not retried
        case other: Throwable =>
          throw other
      }
    }

    // First attempt has no prior error; provide a dummy Throwable
    loop(attempt = 1, lastError = new RuntimeException("No attempts yet"))
  }

  /** Helper that opens a connection, and throws:
    *   - TransientHttpException for transient statuses
    *   - HttpStatusException for all other non-2xx statuses
    *
    * You can use this inside `withTransientHttpRetries` to ensure retry behavior is based on HTTP status.
    */
  def openAndCheck(url: URL): HttpURLConnection = {
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]

    // Trigger the request; for GET, calling getResponseCode initiates the connection
    val status = conn.getResponseCode
    if (status >= 200 && status <= 299) {
      conn
    } else {
      val msg = Option(conn.getResponseMessage).getOrElse("no message")
      // Ensure connection resources are released on failure
      conn.disconnect()
      if (TransientStatusCodes.contains(status)) throw TransientHttpException(status, url, msg)
      else throw HttpStatusException(status, url, msg)
    }
  }

  /** Downloads a URL to a local file with retries for transient HTTP statuses and IOExceptions. */
  def downloadWithRetries(
    url: URL,
    localFile: File,
    maxRetries: Int = 5,
    baseInterval: FiniteDuration = 500.millis,
    backoffFactor: Double = 2.0
  ): Unit = {
    withTransientHttpRetries(maxRetries, baseInterval, backoffFactor) {
      val conn = openAndCheck(url)
      try {
        sbt.io.Using.bufferedInputStream(conn.getInputStream) { inputStream =>
          sbt.IO.transfer(inputStream, localFile)
        }
      } finally conn.disconnect()
    }
  }
}
