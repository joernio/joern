package io.joern.rubysrc2cpg.utils

import better.files.File
import io.joern.x2cpg.utils.FileUtil.*
import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.joern.rubysrc2cpg.parser.RubyAstGenRunner
import io.joern.rubysrc2cpg.passes.{Defines, DependencyPass}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg, parser}
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, FileUtil}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Dependency
import io.shiftleft.semanticcpg.language.*
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.slf4j.LoggerFactory
import upickle.default.*

import java.io.FileOutputStream
import java.net.{HttpURLConnection, URI, URL, URLConnection}
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{GZIPInputStream, InflaterInputStream}
import scala.util.{Failure, Success, Try, Using}
import scala.jdk.CollectionConverters._

/** Queries Ruby Gems for the artifacts of a programs' dependencies, according to the V1 API.
  *
  * @see
  *   <a href="https://guides.rubygems.org/rubygems-org-api/">Ruby Gems API</a>
  */
class DependencyDownloader(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  private val RESOLVER_BASE_URL =
    cpg.dependency.name(Defines.Resolver).version.headOption.getOrElse("https://rubygems.org")

  /** Downloads dependencies and summarises their symbol information.
    * @return
    *   the dependencies' summary combined with the given internal program summary.
    */
  def download(): RubyProgramSummary = {
    File.temporaryDirectory("joern-rubysrc2cpg").apply { dir =>
      cpg.dependency
        .filterNot(dep =>
          dep.name == Defines.Resolver ||
            (DependencyPass.CORE_GEMS.contains(dep.name) && DependencyPass.CORE_GEM_VERSION == dep.version)
        )
        .foreach { dependency =>
          Try(Thread.sleep(100)) // Rate limit
          downloadDependency(dir.path, dependency)
        }
      untarDependencies(dir.path)
      summarizeDependencies(dir.path / "lib")
    }
  }

  private case class RubyGemLatestVersion(version: String) derives ReadWriter

  /** Downloads the dependency to a temporary directory.
    *
    * @param targetDir
    *   the directory to download to.
    * @param dependency
    *   the dependency to download.
    * @return
    *   a disposable version of the directory where the dependencies live.
    */
  private def downloadDependency(targetDir: Path, dependency: Dependency): Unit = {
    def getVersion(packageName: String): Option[String] = {
      Using.resource(URI(s"$RESOLVER_BASE_URL/api/v1/versions/$packageName/latest.json").toURL.openStream()) { is =>
        Try(read[RubyGemLatestVersion](ujson.Readable.fromByteArray(is.readAllBytes()))).toOption
          .map(_.version)
      }
    }

    def createUrl(version: String): URL = {
      URI(s"$RESOLVER_BASE_URL/gems/${dependency.name}-$version.gem").toURL
    }

    // If dependency version is not specified, latest is returned
    val versionOpt = if dependency.version.isBlank then getVersion(dependency.name) else Option(dependency.version)

    versionOpt match {
      case Some(version) =>
        (targetDir / dependency.name).createWithParentsIfNotExists(asDirectory = true)
        downloadPackage(targetDir / dependency.name, dependency, createUrl(version))
      case None =>
        logger.error(s"Unable to determine package version for ${dependency.name}, skipping")
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
  private def downloadPackage(targetDir: Path, dependency: Dependency, url: URL): Unit = {
    var connection: Option[HttpURLConnection] = None
    try {
      connection = Option(url.openConnection()).collect { case x: HttpURLConnection => x }
      // allow both GZip and Deflate (ZLib) encodings
      connection.foreach(_.setRequestProperty("Accept-Encoding", "gzip, deflate"))
      connection match {
        case Some(conn: HttpURLConnection) if conn.getResponseCode == HttpURLConnection.HTTP_OK =>
          val ext      = "gem"
          val fileName = targetDir / s"${dependency.name}.$ext"

          val inputStream = Option(conn.getContentEncoding) match {
            case Some(encoding) if encoding.equalsIgnoreCase("gzip")    => GZIPInputStream(conn.getInputStream)
            case Some(encoding) if encoding.equalsIgnoreCase("deflate") => InflaterInputStream(conn.getInputStream)
            case _                                                      => conn.getInputStream
          }

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

  /** Unzips all the `gem` files and extracts the Ruby source files.
    *
    * @param targetDir
    *   the temporary directory containing all of the successfully downloaded dependencies.
    */
  private def untarDependencies(targetDir: Path): Unit = {
    Files.list(targetDir).iterator().asScala.foreach { pkgDir =>
      Files.list(pkgDir).iterator().asScala.foreach { pkg =>
        {
          Using.resource(Files.newInputStream(pkg)) { pkgIs =>
            // Will unzip to `targetDir/lib` and clean-up
            val tarGemStream = new TarArchiveInputStream(pkgIs)
            Iterator
              .continually(tarGemStream.getNextEntry)
              .takeWhile(_ != null)
              .filter(_.getName == "data.tar.gz")
              .foreach { _ =>
                val gzStream      = new GZIPInputStream(tarGemStream)
                val dataTarStream = new TarArchiveInputStream(gzStream)
                Iterator
                  .continually(dataTarStream.getNextEntry)
                  .takeWhile(_ != null)
                  .filter(sourceEntry =>
                    val entryName = sourceEntry.getName
                    !entryName.contains("..") && entryName.startsWith("lib/") && entryName.endsWith(".rb")
                  )
                  .foreach { rubyFile =>
                    try {
                      val fName  = s"lib/${pkgDir.getFileName.toString}/${rubyFile.getName.stripPrefix("lib/")}"
                      val target = targetDir / fName
                      target.createWithParentsIfNotExists(createParents = true)
                      Using.resource(new FileOutputStream(target.toString)) { fos =>
                        val buffer = new Array[Byte](4096)
                        Iterator
                          .continually(dataTarStream.read(buffer))
                          .takeWhile(_ != -1)
                          .foreach(bytesRead => fos.write(buffer, 0, bytesRead))
                      }
                    } catch {
                      case exception: Throwable =>
                        logger.error(s"Exception occurred while unpacking ${rubyFile.getName}", exception)
                    }
                  }
              }
          }
          FileUtil.delete(pkg, swallowIoExceptions = true)
        }
        FileUtil.delete(pkgDir, swallowIoExceptions = true)
      }
    }
  }

  /** Given a directory of all the summaries, will produce a summary thereof.
    * @param targetDir
    *   the directory with all the dependencies.
    * @return
    *   a summary of all the dependencies.
    */
  private def summarizeDependencies(targetDir: Path): RubyProgramSummary = {

    /** Map the path to a non-relative form as would be accessed from the application.
      * @param libSummary
      *   the library summary to re-map.
      */
    def remapPaths(libSummary: RubyProgramSummary): RubyProgramSummary = {
      val pathMappings = libSummary.pathToType.map { case (key, typs) => key.stripPrefix("./") -> typs }
      RubyProgramSummary(libSummary.namespaceToType, pathMappings)
    }

    val tmpDir = Files.createTempDirectory("rubysrc2cpgOut")

    try {
      val config       = Config().withDisableFileContent(true)
      val astGenResult = RubyAstGenRunner(Config().withInputPath(targetDir.toString)).execute(tmpDir)

      val astCreators = ConcurrentTaskUtil
        .runUsingThreadPool(
          RubySrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config, Option(targetDir.toString))
        )
        .flatMap {
          case Failure(exception)  => logger.warn(s"Unable to parse Ruby file, skipping -", exception); None
          case Success(astCreator) => Option(astCreator)
        }
        .filter(x => {
          if x.fileContent.isBlank then logger.info(s"File content empty, skipping - ${x.fileName}")
          !x.fileContent.isBlank
        })

      // Pre-parse the AST creators for high level structures
      val librarySummaries = ConcurrentTaskUtil
        .runUsingThreadPool(astCreators.map(x => () => remapPaths(x.summarize(asExternal = true))).iterator)
        .flatMap {
          case Failure(exception) => logger.warn(s"Unable to pre-parse Ruby file, skipping - ", exception); None
          case Success(summary)   => Option(summary)
        }
        .reduceOption((a, b) => a ++= b)
        .getOrElse(RubyProgramSummary())

      librarySummaries
    } finally {
      FileUtil.delete(tmpDir, swallowIoExceptions = true)
    }
  }

}
