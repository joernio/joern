package io.joern.csharpsrc2cpg.utils

import better.files.File
import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
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

/** Queries NuGet for the artifacts of a programs' dependencies, according to the V2 API.
  *
  * TODO: Note that NuGet has API throttling, so beware of that if this is to be processed concurrently.
  *
  * @see
  *   <a href="https://learn.microsoft.com/en-us/nuget/api/overview">NuGet API</a>
  */
class DependencyDownloader(
  cpg: Cpg,
  config: Config,
  internalProgramSummary: CSharpProgramSummary,
  internalPackages: Set[String] = Set.empty
) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val NUGET_BASE_API_V2 = "www.nuget.org/api/v2"
  private val NUGET_BASE_API_V3 = "api.nuget.org/v3-flatcontainer"

  /** Downloads dependencies and summarises their symbol information.
    * @return
    *   the dependencies' summary combined with the given internal program summary.
    */
  def download(): CSharpProgramSummary = {
    File.temporaryDirectory("joern-csharpsrc2cpg").apply { dir =>
      cpg.dependency.filterNot(isAlreadySummarized).foreach(downloadDependency(dir, _))
      unzipDependencies(dir)
      summarizeDependencies(dir) ++ internalProgramSummary
    }
  }

  /** Using the internal program summary (which may contain pre-defined type stubs), determines if the dependency is
    * already summarized and does not need to be downloaded.
    * @param dependency
    *   the dependency to download.
    * @return
    *   true if the dependency is already in the given summary, false if otherwise.
    */
  private def isAlreadySummarized(dependency: Dependency): Boolean = {
    // TODO: `namespace` != `packageId`, so we may want to have summaries store the `packageId` in future
    internalProgramSummary.namespaceToType.keySet.exists(_.startsWith(dependency.name)) || internalPackages.contains(
      dependency.name
    )
  }

  private case class NuGetPackageVersions(versions: List[String]) derives ReadWriter

  /** Downloads the dependency to a temporary directory. This will be a `nupkg` and `snupkg` file for each dependency
    * respectively, which are compressed using the ZIP format.
    *
    * @param targetDir
    *   the directory to download to.
    * @param dependency
    *   the dependency to download.
    * @return
    *   a disposable version of the directory where the dependencies live.
    */
  private def downloadDependency(targetDir: File, dependency: Dependency): Unit = {

    val dependencyName = dependency.name.strip()
    def getVersion(packageName: String): Option[String] = Try {
      Using.resource(URI(s"https://$NUGET_BASE_API_V3/${packageName.toLowerCase}/index.json").toURL.openStream()) {
        is =>
          Try(read[NuGetPackageVersions](ujson.Readable.fromByteArray(is.readAllBytes()))).toOption
            .flatMap(_.versions.lastOption)
      }
    } match {
      case Failure(_) =>
        logger.error(s"Unable to resolve `index.json` for `$packageName`, skipping...`")
        None
      case Success(x) => x
    }

    def createUrl(packageType: String, version: String): URL = {
      URI(s"https://$NUGET_BASE_API_V2/$packageType/${dependencyName}/$version").toURL
    }

    // If dependency version is not specified, latest is returned
    val versionOpt =
      if dependency.version.isBlank then getVersion(dependencyName) else Option(dependency.version)

    versionOpt match {
      case Some(version) =>
        downloadPackage(targetDir, dependency, createUrl("package", version))
        downloadPackage(targetDir, dependency, createUrl("symbolpackage", version))
      case None =>
        logger.error(s"Unable to determine package version for ${dependencyName}, skipping")
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
          val ext      = if url.toString.contains("/package/") then "nupkg" else "snupkg"
          val fileName = targetDir / s"${dependency.name}.$ext"

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

  /** Given a directory of all the summaries, will produce a summary thereof.
    * @param targetDir
    *   the directory with all the dependencies.
    * @return
    *   a summary of all the dependencies.
    */
  private def summarizeDependencies(targetDir: File): CSharpProgramSummary = {
    val astGenRunner       = new DotNetAstGenRunner(config.withInputPath(targetDir.pathAsString))
    val astGenRunnerResult = astGenRunner.execute(targetDir)
    val summaries = astGenRunnerResult.parsedFiles
      .map(x => File(x))
      .flatMap { f =>
        Using.resource(f.newFileInputStream) { fis =>
          CSharpProgramSummary.jsonToInitialMapping(fis) match {
            case Failure(exception) =>
              logger.error(s"Unable to parse JSON program summary at $f", exception)
              None
            case Success(parsedJson) =>
              Option(parsedJson)
          }
        }
      }
      .map(CSharpProgramSummary(_))
    CSharpProgramSummary(summaries)
  }

}
