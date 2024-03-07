package io.joern.csharpsrc2cpg.utils

import better.files.File
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.semanticcpg.utils.SecureXmlParsing
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Dependency
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.io.{BufferedReader, FileOutputStream, InputStreamReader}
import java.net.{HttpURLConnection, URI, URL, URLConnection}
import java.util.zip.{GZIPInputStream, InflaterInputStream, ZipEntry}
import scala.util.{Failure, Success, Try, Using}
import scala.xml.Elem

/** Queries NuGet for the artifacts of a programs' dependencies.
  *
  * @see
  *   <a href="https://learn.microsoft.com/en-us/nuget/api/overview">NuGet API</a>
  */
class DependencyDownloader(
  cpg: Cpg,
  internalProgramSummary: CSharpProgramSummary,
  nugetRepositoryBaseUrl: String = "www.nuget.org",
  nugetApiVersion: String = "v2"
) {

  private val logger = LoggerFactory.getLogger(getClass)

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
    // TODO: Implement
    false
  }

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
    Seq("package", "symbolpackage")
      .map(packageType =>
        URI(
          s"https://$nugetRepositoryBaseUrl/api/$nugetApiVersion/$packageType/${dependency.name}/${dependency.version}"
        ).toURL
      )
      .foreach { url =>
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
            case _ => logger.error(s"Unknown URL connection made, aborting")
          }

        } catch {
          case exception: Throwable =>
            logger.error(s"Unable to download dependency ${dependency.name}:${dependency.version}", exception)
        } finally {
          connection.foreach(_.disconnect())
        }
      }
  }

  /** Unzips all the `pkg` files and extracts the `DLL`, `XML`, and `PDB` files.
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
    // Sometimes these dependencies will include DLLs for multiple version of dotnet, we only want one
    libDir.listRecursively.filterNot(_.isDirectory).distinctBy(_.name).foreach { f =>
      f.copyTo(targetDir / f.name)
    }
    // Clean-up lib dir
    libDir.delete(swallowIOExceptions = true)
  }

  /** Given a directory of all the summaries, will produce a summary thereof.
    * @param targetDir
    *   the directory with all the dependencies.
    * @return
    *   a summary of all the dependencies.
    */
  private def summarizeDependencies(targetDir: File): CSharpProgramSummary = {
    // TODO: Implement
    CSharpProgramSummary()
  }

}
