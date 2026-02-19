package io.joern.x2cpg.utils

import io.shiftleft.semanticcpg.utils.FileUtil
import org.slf4j.LoggerFactory

import java.io.IOException
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.Base64
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, TimeUnit}
import scala.util.{Failure, Success, Try, Using}

case class HttpArtifact(url: String, sha256: String)

case class Credentials(user: String, password: String)

object Credentials {

  def load(path: Path): Seq[Credentials] = {
    if (Files.exists(path)) {
      val props = new java.util.Properties()
      Using.resource(Files.newInputStream(path)) { is =>
        props.load(is)
      }

      val user     = Option(props.getProperty("user")).getOrElse("")
      val password = Option(props.getProperty("password")).getOrElse("")

      if (user.nonEmpty) {
        Seq(Credentials(user, password))
      } else {
        Seq.empty
      }
    } else {
      Seq.empty
    }
  }

  def fromEnv(): Option[Credentials] = {
    for {
      user <- sys.env.get("X2CPG_SBT_CREDENTIALS_USER")
      pass <- sys.env.get("X2CPG_SBT_CREDENTIALS_PASS")
    } yield Credentials(user = user, password = pass)
  }
}

class ArtifactFetcher(val cacheDir: Path) {
  import ArtifactFetcher.*

  private val inFlight: ConcurrentHashMap[String, CompletableFuture[Option[Path]]] =
    new ConcurrentHashMap[String, CompletableFuture[Option[Path]]]()

  def fetch(artifact: HttpArtifact): Option[Path] = {
    val urlHash = HashUtil.sha256(artifact.url.getBytes("UTF-8"))

    val fileName = filenameFromUrl(artifact.url)
    val entryDir = cacheDir.resolve(urlHash)
    val target   = entryDir.resolve(fileName)

    val future = new CompletableFuture[Option[Path]]()
    Option(inFlight.putIfAbsent(artifact.url, future)) match {
      case Some(existingFuture) =>
        Try(existingFuture.get(DownloadTimeoutMinutes, TimeUnit.MINUTES)) match {
          case Success(result) => result
          case Failure(e) =>
            logger.warn(s"Wait for existing download of ${artifact.url} failed or timed out: ${e.getMessage}")
            None
        }
      case None =>
        try {
          Try(cachedPath(artifact, target).orElse(download(artifact, entryDir, target))) match {
            case Success(result) =>
              future.complete(result)
              result
            case Failure(e: IOException) =>
              future.completeExceptionally(e)
              throw e
            case Failure(t) =>
              logger.error(s"Critical failure fetching ${artifact.url}", t)
              future.completeExceptionally(t)
              None
          }
        } finally {
          inFlight.remove(artifact.url)
        }
    }
  }

  private def cachedPath(artifact: HttpArtifact, target: Path): Option[Path] = {
    try {
      if (Files.exists(target) && HashUtil.sha256(target) == artifact.sha256) {
        logger.debug(s"Cache hit for ${artifact.url}")
        Some(target)
      } else {
        if (Files.exists(target)) logger.info(s"Cached file hash mismatch for ${artifact.url}, re-downloading")
        None
      }
    } catch {
      case e: Exception =>
        logger.warn(s"Error checking cache for ${artifact.url}: ${e.getMessage}")
        None
    }
  }

  private def download(artifact: HttpArtifact, entryDir: Path, target: Path): Option[Path] = {
    logger.info(s"Downloading ${artifact.url}")

    Try(Files.createDirectories(entryDir)) match {
      case Failure(e) =>
        logger.warn(s"Failed to create cache directory for ${artifact.url}: ${e.getMessage}")
        None
      case _ =>
        FileUtil.usingTemporaryFile(prefix = "x2cpgTestArtifact_", suffix = ".tmp") { tmpFile =>
          Try {
            val requestBuilder = HttpRequest.newBuilder().uri(URI.create(artifact.url)).GET()
            findCredentials(artifact.url).foreach { cred =>
              val encoded = Base64.getEncoder.encodeToString(s"${cred.user}:${cred.password}".getBytes("UTF-8"))
              requestBuilder.header("Authorization", s"Basic $encoded")
            }
            val request  = requestBuilder.build()
            val response = httpClient.send(request, HttpResponse.BodyHandlers.ofFile(tmpFile))

            val status = response.statusCode()
            if (status != 200) {
              val msg = s"Status code $status"
              if (RetryableStatusCodes.contains(status)) {
                throw new IOException(msg)
              } else {
                throw new RuntimeException(msg)
              }
            }

            val downloadedHash = HashUtil.sha256(tmpFile)
            if (downloadedHash != artifact.sha256) {
              throw new RuntimeException(s"Hash mismatch: expected ${artifact.sha256}, got $downloadedHash")
            }

            Files.move(tmpFile, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
            logger.debug(s"Cached ${artifact.url} at $target")
            target
          } match {
            case Failure(e: IOException) =>
              throw e
            case Failure(e) =>
              logger.warn(s"Failed to download/verify ${artifact.url}: ${e.getMessage}")
              None
            case Success(path) =>
              Some(path)
          }
        }
    }
  }

}

object ArtifactFetcher {

  private val logger                 = LoggerFactory.getLogger(getClass)
  private val DownloadTimeoutMinutes = 10
  private val RetryableStatusCodes   = Set(408, 429, 500, 502, 503, 504)

  private lazy val httpClient: HttpClient =
    HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()

  private def defaultCacheDir: Path = {
    val dir = sys.env
      .get("X2CPG_TEST_ARTIFACT_CACHE")
      .map(Path.of(_))
      .getOrElse(Path.of(System.getProperty("user.home"), ".shiftleft", "test-artifacts"))
    Files.createDirectories(dir)
    dir
  }

  private def credentialsPath: Path = {
    sys.env
      .get("X2CPG_CREDENTIALS_FILE")
      .map(Path.of(_))
      .getOrElse(Path.of(System.getProperty("user.home"), ".sbt", ".credentials"))
  }

  private def filenameFromUrl(url: String): String = {
    val path = URI.create(url).getPath
    val name = path.substring(path.lastIndexOf('/') + 1)
    if (name.isEmpty) "artifact" else name
  }

  private def findCredentials(url: String): Option[Credentials] = {
    // TODO: This assumes credentials for only a single realm/host
    Credentials.fromEnv().orElse(Credentials.load(credentialsPath).headOption)
  }

  def apply(cacheDir: Path): ArtifactFetcher = new ArtifactFetcher(cacheDir)

  private lazy val defaultInstance: ArtifactFetcher = new ArtifactFetcher(defaultCacheDir)

  def fetch(artifact: HttpArtifact): Option[Path] = defaultInstance.fetch(artifact)
}
