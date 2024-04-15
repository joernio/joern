package io.joern.php2cpg.utils

import better.files.File
import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.x2cpg.astgen.AstGenRunner.DefaultAstGenRunnerResult
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Dependency
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.FileOutputStream
import java.net.{HttpURLConnection, URI, URL, URLConnection}
import java.util.zip.{GZIPInputStream, InflaterInputStream, ZipEntry}
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
    unzipDependencies(dir)
    dir
  }

  private def isAutoloadedDependency(dependency: Dependency): Boolean = {
    dependency.version.startsWith(PhpOperators.autoload)
  }

  private case class Dist(reference: String, shasum: String, `type`: String, url: URL) derives ReadWriter

  private case class Package(dist: Dist, version_normalized: String) derives ReadWriter

  private case class PackagistResponse(packages: Map[String, Package]) derives ReadWriter

  /** Downloads the dependency to a temporary directory. This will be a `zip` file for each dependency respectively.
    *
    * @param targetDir
    *   the directory to download to.
    * @param dependency
    *   the dependency to download.
    */
  private def downloadDependency(targetDir: File, dependency: Dependency): Unit = {

    val dependencyName    = dependency.name.strip()
    val dependencyVersion = dependency.version // TODO: Parse properly

    dependencyName.split("\\\\").toList match {
      case vendor :: pack :: Nil =>
        val uri = URI(s"https://$PACKAGIST_API/$vendor/$pack.json").toURL
        downloadPackage(targetDir, dependency, uri)
      case xs =>
        logger.warn(s"Ignoring package ${xs.mkString("\\")} as vendor and package name cannot be distinguished")
    }
  }

  /** Downloads the package and unpacks the contents to the target directory.
    * @param targetDir
    *   the directory to download and unpack to.
    * @param dependency
    *   the dependency information.
    * @param url
    *   the download URL.
    * @return
    *   the package version.
    */
  private def downloadPackage(targetDir: File, dependency: Dependency, url: URL): Unit = {
    var connection: Option[HttpURLConnection] = None
    try {
      connection = Option(url.openConnection()).collect { case x: HttpURLConnection => x }
      // allow both GZip and Deflate (ZLib) encodings
      connection.foreach(_.setRequestProperty("Accept-Encoding", "gzip, deflate"))
      connection match {
        case Some(conn: HttpURLConnection) if conn.getResponseCode == HttpURLConnection.HTTP_OK =>
          val fileName = targetDir / s"${dependency.name}.zip"

          val inputStream = Option(conn.getContentEncoding) match {
            case Some(encoding) if encoding.equalsIgnoreCase("gzip")    => GZIPInputStream(conn.getInputStream)
            case Some(encoding) if encoding.equalsIgnoreCase("deflate") => InflaterInputStream(conn.getInputStream)
            case _                                                      => conn.getInputStream
          }

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

  /** Unzips all the `pkg` files and extracts the `DLL`, `XML`, and `PDB` files.
    *
    * @param targetDir
    *   the temporary directory containing all of the successfully downloaded dependencies.
    */
  private def unzipDependencies(targetDir: File): Unit = {

    def zipFilter(zipEntry: ZipEntry): Boolean = {
      val isZipSlip = zipEntry.getName.contains("..")
      !isZipSlip && (zipEntry.isDirectory || zipEntry.getName.matches(".*lib.*\\.(dll|xml|pdb)$"))
    }

    targetDir.list.foreach { pkg =>
      // Will unzip to `targetDir/lib` and clean-up
      pkg.unzipTo(targetDir, zipFilter)
      pkg.delete(swallowIOExceptions = true)
    }

    // Move and merge files
    val libDir = targetDir / "lib"
    if (libDir.isDirectory) {
      // Sometimes these dependencies will include DLLs for multiple version of dotnet, we only want one
      libDir.listRecursively.filterNot(_.isDirectory).distinctBy(_.name).foreach { f =>
        f.copyTo(targetDir / f.name)
      }
      // Clean-up lib dir
      libDir.delete(swallowIOExceptions = true)
    }
  }

}
