package io.joern.kotlin2cpg.jar4import

import com.squareup.tools.maven.resolution.ArtifactResolver
import io.joern.kotlin2cpg.Kotlin2Cpg
import org.slf4j.LoggerFactory

import java.net.{MalformedURLException, URL}

trait UsesService { this: Kotlin2Cpg =>

  private val logger = LoggerFactory.getLogger(getClass)

  protected def reachableServiceMaybe(serviceUrl: String): Option[Service] = {
    try {
      val url            = new URL(serviceUrl)
      val healthResponse = requests.get(url.toString + "/health")
      if (healthResponse.statusCode != 200) {
        println(s"The jar4import service at `${url.toString}` did not respond with 200 on the `/health` endpoint.")
        System.exit(1)
      }
      Some(new Service(url.toString))
    } catch {
      case _: MalformedURLException =>
        println(s"The specified jar4import service url parameter `$serviceUrl` is not a valid URL. Exiting.")
        System.exit(1)
        None
      case _: java.net.ConnectException =>
        println(s"Could not connect to service at url `$serviceUrl`. Exiting.")
        System.exit(1)
        None
      case _: requests.RequestFailedException =>
        println(s"Request to `$serviceUrl` failed. Exiting.")
        System.exit(1)
        None
    }
  }

  protected def dependenciesFromService(service: Service, importNames: Seq[String]): Seq[String] = {
    try {
      val coordinates = service.fetchDependencyCoordinates(importNames)
      logger.debug(s"Found coordinates `$coordinates`.")

      val resolver = new ArtifactResolver()
      val artifacts = coordinates.map { coordinate =>
        val strippedCoordinate = coordinate.stripPrefix("\"").stripSuffix("\"")
        val result             = resolver.download(strippedCoordinate, true)
        logger.debug(s"Downloaded artifact for coordinate `$strippedCoordinate`.")
        result.component2().toAbsolutePath.toString
      }
      logger.info(s"Using `${artifacts.size}` dependencies.")

      artifacts
    } catch {
      case e: Throwable =>
        logger.info("Caught exception while downloading dependencies", e)
        System.exit(1)
        Seq()
    }
  }

}
