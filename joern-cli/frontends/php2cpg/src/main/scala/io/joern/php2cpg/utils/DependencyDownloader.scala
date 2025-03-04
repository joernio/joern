package io.joern.php2cpg.utils

import better.files.File
import com.github.sh4869.semver_parser.{Range, SemVer}
import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.passes.{Composer, PsrArray, PsrString}
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Dependency
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.{FileOutputStream, IOException}
import java.nio.file.{CopyOption, Files, Path, Paths, StandardCopyOption}
import java.net.{HttpURLConnection, URI, URL, URLConnection}
import java.util.zip.ZipEntry
import javax.json.JsonException
import scala.util.{Failure, Success, Try, Using}
import scala.jdk.CollectionConverters.*

/** Queries <a href="https://packagist.org">Packgist</a> for dependencies and downloads them.
  */
class DependencyDownloader(cpg: Cpg, config: Config) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val PACKAGIST_API = "repo.packagist.org/p2"

  /** Downloads dependencies and places them in a temporary directory.
    * @return
    *   the path to the directory of downloaded dependencies. This directory is deleted on exit.
    */
  def download(): Path = {
    val dir = Files.createTempDirectory("joern-php2cpg")
    FileUtil.deleteOnExit(dir, swallowIOExceptions = true)
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
  private def downloadDependency(targetDir: Path, dependency: Dependency): Unit = {
    Thread.sleep(100) // throttling

    val dependencyName = dependency.name.strip()
    // Fall back for exceptional cases like "abc/def/ghi" or "abc-def#8def05" etc.
    val dependencyRange =
      try {
        Range(dependency.version)
      } catch {
        case _: Throwable => Range(">=0.0.0")
      }

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
      case Success(x) =>
        x.packages.flatMap(_._2).find { x =>
          Try(dependencyRange.valid(x.version)) match {
            case Failure(exception) =>
              logger.error(s"Unable to determine if $x is valid for given range $dependencyRange", exception)
              false
            case Success(value) => value
          }
        }
    }

    val (vendor, packName) = dependencyName.split("/").toList match {
      case vendor :: packName :: xs => (vendor, packName)
      case packName :: Nil          => (packName, packName)
      case _                        => (dependencyName, dependencyName)
    }

    Try {
      getCompatiblePackage(vendor, packName).foreach { pack =>
        downloadPackage(targetDir, dependency, pack)
          .foreach(_ => unzipDependency(targetDir, pack, vendor))
      }
    } match {
      case Failure(exception) =>
        logger.error("Exception encountered while downloading and unzipping dependency.", exception)
      case Success(_) => logger.info(s"Successfully downloaded $vendor/$packName")
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
    *   success if package was successfully downloaded, a failure if otherwise.
    */
  private def downloadPackage(targetDir: Path, dependency: Dependency, pack: Package): Try[Unit] = Try {
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
            Using.resources(inputStream, new FileOutputStream(fileName.toString)) { (is, fos) =>
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
              throw exception
            case Success(_) =>
              logger.info(s"Successfully downloaded dependency ${dependency.name}:${dependency.version}")
          }
        case Some(conn: HttpURLConnection) =>
          logger.error(s"Connection to $url responded with non-200 code ${conn.getResponseCode}")
          throw new RuntimeException()
        case _ =>
          logger.error(s"Unknown URL connection made, aborting")
          throw new RuntimeException()
      }
    } catch {
      case exception: Throwable =>
        logger.error(s"Unable to download dependency ${dependency.name}:${dependency.version}", exception)
        throw exception
    } finally {
      connection.foreach(_.disconnect())
    }
  }

  /** Unzips all the `zip` files and extracts the `php` files.
    *
    * @param targetDir
    *   the temporary directory containing all of the successfully downloaded dependencies.
    */
  private def unzipDependency(targetDir: Path, pack: Package, vendor: String): Unit = {
    def zipFilter(zipEntry: ZipEntry) = {
      val isZipSlip = zipEntry.getName.contains("..")
      !isZipSlip && (zipEntry.isDirectory || zipEntry.getName.matches(".*\\.(php|json)$"))
    }

    def moveDir(targetNamespace: String, pathPrefix: String): Unit = {
      val fullTargetNamespace = (targetDir / targetNamespace
        .replace("\\", java.io.File.separator))
        .createWithParentsIfNotExists(asDirectory = true, createParents = true)
      val fullPathPrefix = targetDir / pathPrefix.replace("/", java.io.File.separator)

      Files.list(fullPathPrefix).forEach { x =>
        Files.move(x, fullTargetNamespace / x.getFileName.toString)
      }
      FileUtil.delete(fullPathPrefix, swallowIoExceptions = true)
    }

    targetDir.listFiles().filterNot(Files.isDirectory(_)).foreach { pkg =>
      pkg.unzipTo(targetDir, zipFilter)
      FileUtil.delete(pkg, swallowIoExceptions = true)

      targetDir
        .listFiles()
        .filter(f => Files.isDirectory(f) && f.getFileName.toString.startsWith(vendor.replace("\\", "-")))
        .foreach { unpackedDest =>
          unpackedDest.mergeDirectory(targetDir, copyOptions = StandardCopyOption.REPLACE_EXISTING)
          FileUtil.delete(unpackedDest, swallowIoExceptions = true)
        }
    }

    targetDir
      .walk()
      .collectFirst {
        case x if x.getFileName.toString == "composer.json" =>
          Using.resource(Files.newInputStream(x)) { is =>
            read[Composer](ujson.Readable.fromByteArray(is.readAllBytes()))
          }
      }
      .foreach { composer =>
        composer.autoload.`psr-0`.foreach {
          case (targetNamespace, PsrString(pathPrefix))  => moveDir(targetNamespace, pathPrefix)
          case (targetNamespace, PsrArray(pathPrefixes)) => pathPrefixes.foreach(moveDir(targetNamespace, _))
        }
        composer.autoload.`psr-4`.foreach {
          case (targetNamespace, PsrString(pathPrefix))  => moveDir(targetNamespace, pathPrefix)
          case (targetNamespace, PsrArray(pathPrefixes)) => pathPrefixes.foreach(moveDir(targetNamespace, _))
        }
      }

    // Clean up `.json` files
    targetDir
      .listFiles()
      .filter(_.toString.endsWith(".json"))
      .foreach(FileUtil.delete(_, swallowIoExceptions = true))
  }

}

implicit val urlRw: ReadWriter[URL] = readwriter[ujson.Value]
  .bimap[URL](
    x => ujson.Str(x.toString),
    {
      case json @ (j: ujson.Str) => URI(json.str).toURL
      case x                     => throw JsonException(s"Unexpected value type for URL strings: ${x.getClass}")
    }
  )

implicit val semverRw: ReadWriter[SemVer] = readwriter[ujson.Value]
  .bimap[SemVer](
    x => ujson.Str(x.toString),
    {
      case json @ (j: ujson.Str) => json.str.asSemver
      case x                     => throw JsonException(s"Unexpected value type for URL strings: ${x.getClass}")
    }
  )

private case class Dist(reference: String, shasum: String, `type`: String, url: URL) derives ReadWriter

private case class Package(dist: Dist, @upickle.implicits.key("version_normalized") version: SemVer) derives ReadWriter

implicit val packageRw: ReadWriter[PackageReleases] = readwriter[ujson.Value].bimap[PackageReleases](
  x => ujson.Obj("packages" -> write(x.packages)),
  json =>
    val packagesWithDist = json("packages").obj.map { case (packName, packages) =>
      packName -> packages.arr.filter(_.obj.keySet.contains("dist"))
    }
    PackageReleases(read[Map[String, List[Package]]](packagesWithDist))
)

private case class PackageReleases(packages: Map[String, List[Package]])

/** Parses and simplifies the version string to help prevent the flakey SemVer parser from crashing.
  */
implicit class SemverStrExt(s: String) {

  def asSemver: SemVer = {
    val stableSemverString =
      s.replaceAll("^[vV]", "").replaceAll("[+-].*$", "") // get rid of leading `v` or trailing build info

    // Packagist normalized versions have a 4th version number entry which we can discard
    if (stableSemverString.count(_ == '.') == 3) {
      SemVer(stableSemverString.substring(0, stableSemverString.lastIndexOf('.')))
    } else {
      SemVer(stableSemverString)
    }
  }

}
