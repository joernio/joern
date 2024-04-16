package io.joern.php2cpg.utils

import better.files.File
import com.github.sh4869.semver_parser.{Range, SemVer}
import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.passes.{Autoload, Composer}
import io.joern.x2cpg.astgen.AstGenRunner.DefaultAstGenRunnerResult
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Dependency
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.{FileOutputStream, IOException}
import java.net.{HttpURLConnection, URI, URL, URLConnection}
import java.util.zip.{GZIPInputStream, InflaterInputStream, ZipEntry}
import javax.json.JsonException
import scala.util.{Failure, Success, Try, Using}

/** Queries <a href="https://packagist.org">Packgist</a> for dependencies and downloads them.
  */
class DependencyDownloader(cpg: Cpg, config: Config) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val PACKAGIST_API = "repo.packagist.org/p2"

  /** Downloads dependencies and places them in a temporary directory.
    * @return
    *   the path to the directory of downloaded dependencies. This directory is deleted on exit.
    */
  def download(): File = {
    val dir = File.newTemporaryDirectory("joern-php2cpg").deleteOnExit(swallowIOExceptions = true)
    cpg.dependency.filterNot(isAutoloadedDependency).foreach(downloadDependency(dir, _))
    dir
  }

  private def isAutoloadedDependency(dependency: Dependency): Boolean = {
    dependency.version.startsWith(PhpOperators.autoload)
  }

  /** Downloads the dependency to a temporary directory. This will be a `zip` file for each dependency respectively.
    *
    * @param targetDir
    *   the directory to download to.
    * @param dependency
    *   the dependency to download.
    */
  private def downloadDependency(targetDir: File, dependency: Dependency): Unit = {
    Thread.sleep(100) // throttling

    val dependencyName  = dependency.name.strip()
    val dependencyRange = Range(dependency.version)

    def getCompatiblePackage(vendor: String, pack: String): Option[Package] = Try {
      Using.resource(URI(s"https://$PACKAGIST_API/$vendor/$pack.json").toURL.openStream()) { is =>
        read[PackageReleases](ujson.Readable.fromByteArray(is.readAllBytes()))
      }
    } match {
      case Failure(_: IOException) =>
        logger.error(s"Unable to resolve package information for `$vendor/$pack`, skipping...`")
        None
      case Failure(e) =>
        logger.error(s"Unable to handle package information `$vendor/$pack`, skipping...`", e)
        None
      case Success(x) => x.packages.flatMap(_._2).collectFirst { case x if dependencyRange.valid(x.version) => x }
    }

    dependencyName.split("/").toList match {
      case vendor :: pack :: Nil =>
        getCompatiblePackage(vendor, pack).foreach { pack =>
          downloadPackage(targetDir, dependency, pack)
          unzipDependency(targetDir, pack)
        }
      case xs =>
        logger.warn(s"Ignoring package ${xs.mkString("\\")} as vendor and package name cannot be distinguished")
    }
  }

  /** Downloads the package and unpacks the contents to the target directory.
    * @param targetDir
    *   the directory to download and unpack to.
    * @param dependency
    *   the dependency information.
    * @param pack
    *   the package info.
    * @return
    *   the package version.
    */
  private def downloadPackage(targetDir: File, dependency: Dependency, pack: Package): Unit = {
    var connection: Option[HttpURLConnection] = None
    val url                                   = pack.dist.url
    try {
      connection = Option(url.openConnection()).collect { case x: HttpURLConnection => x }
      // allow both GZip and Deflate (ZLib) encodings
      connection match {
        case Some(conn: HttpURLConnection) if conn.getResponseCode == HttpURLConnection.HTTP_OK =>
          val fileName = targetDir / s"${dependency.name.split("/").last}.zip"

          val inputStream = conn.getInputStream

          Try {
            Using.resources(inputStream, new FileOutputStream(fileName.pathAsString)) { (is, fos) =>
              val buffer = new Array[Byte](4096)
              Iterator
                .continually(is.read(buffer))
                .takeWhile(_ != -1)
                .foreach(bytesRead => fos.write(buffer, 0, bytesRead))
            }
          } match {
            case Failure(exception) =>
              logger.error(
                s"Exception occurred while downloading $fileName (${dependency.name}:${dependency.version})",
                exception
              )
            case Success(_) =>
              logger.info(s"Successfully downloaded dependency ${dependency.name}:${dependency.version}")
          }
        case Some(conn: HttpURLConnection) =>
          logger.error(s"Connection to $url responded with non-200 code ${conn.getResponseCode}")
        case _ =>
          logger.error(s"Unknown URL connection made, aborting")
      }
    } catch {
      case exception: Throwable =>
        logger.error(s"Unable to download dependency ${dependency.name}:${dependency.version}", exception)
    } finally {
      connection.foreach(_.disconnect())
    }
  }

  /** Unzips all the `zip` files and extracts the `php` files.
    *
    * @param targetDir
    *   the temporary directory containing all of the successfully downloaded dependencies.
    */
  private def unzipDependency(targetDir: File, pack: Package): Unit = {

    def zipFilter(zipEntry: ZipEntry): Boolean = {
      val isZipSlip = zipEntry.getName.contains("..")
      !isZipSlip && (zipEntry.isDirectory || zipEntry.getName.matches(".*\\.(php|json)$"))
    }

    targetDir.list.foreach { pkg =>
      pkg.unzipTo(targetDir, zipFilter)
      pkg.delete(swallowIOExceptions = true)
    }

    // Move and merge files according to `composer.json`
    val composer = targetDir.walk().collectFirst {
      case x if x.name == "composer.json" =>
        Try(Using.resource(x.newInputStream) { is => read[Composer](ujson.Readable.fromByteArray(is.readAllBytes())) })
    }
    // TODO
    val libDir = targetDir / "lib"
    if (libDir.isDirectory) {
      libDir.listRecursively.filterNot(_.isDirectory).distinctBy(_.name).foreach { f =>
        f.copyTo(targetDir / f.name)
      }
      // Clean-up lib dir
      libDir.delete(swallowIOExceptions = true)
    }
  }

}

implicit val urlRw: ReadWriter[URL] = readwriter[ujson.Value]
  .bimap[URL](
    x => ujson.Str(x.toString),
    {
      case json @ (j: ujson.Str) => URL(json.str)
      case x                     => throw JsonException(s"Unexpected value type for URL strings: ${x.getClass}")
    }
  )

implicit val semverRw: ReadWriter[SemVer] = readwriter[ujson.Value]
  .bimap[SemVer](
    x => ujson.Str(x.toString),
    {
      case json @ (j: ujson.Str) => SemVer(json.str)
      case x                     => throw JsonException(s"Unexpected value type for URL strings: ${x.getClass}")
    }
  )

private case class Dist(reference: String, shasum: String, `type`: String, url: URL) derives ReadWriter

private case class Package(dist: Dist, version: SemVer) derives ReadWriter

private case class PackageReleases(packages: Map[String, List[Package]]) derives ReadWriter
