package io.shiftleft.resolver

import cats.Parallel
import cats.effect.*
import cats.effect.std.Console
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.shiftleft.resolver.impl.*
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.client.Client
import org.http4s.client.middleware.FollowRedirect
import org.http4s.ember.client.EmberClientBuilder

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path as JPath

object LocalResolverMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    Configurator.setRootLevel(Level.DEBUG)

    // In this dir files named `pom.xml` are searched.
    val scanDir = Path("/tmp/testData")
    // In this dir the resulting meta data and lib info files are stored.
    val outputDir = JPath.of("/tmp/libInfo")

    val httpClientR = EmberClientBuilder.default[IO].build.map(FollowRedirect(10))
    httpClientR.use { httpClient =>
      val orchestrators = Vector(
        buildOrchestratorMaven(outputDir, "https://repo1.maven.org/maven2", httpClient),
        //Orchestrator(BuildTargetExtractorSwift[IO](), null, null, null)
      )

      val stream =
        associateBuildFiles(Files[IO].walk(scanDir), orchestrators).flatMap {
          case (orchestrator, buildFiles) =>
            orchestrator.run(buildFiles.map(_.toNioPath))
        }

      stream.compile.drain.attempt.flatMap {
        case Left(throwable) =>
          IO.pure {
            val stringWriter = new StringWriter()
            val printWriter = new PrintWriter(stringWriter)
            throwable.printStackTrace(printWriter)
            stringWriter
          }.flatMap { stringWriter =>
            Console[IO].println(stringWriter) >> IO.pure(ExitCode.Error)
          }
        case Right(_) =>
          IO.pure(ExitCode.Success)
      }
    }
  }

  private def associateBuildFiles[F[_]: Sync](fileStream: Stream[F, Path],
                                              extractors: Vector[Orchestrator[F, ?]])
  : Stream[F, (Orchestrator[F, ?], List[Path])] = {
    fileStream.fold(Map.empty[Orchestrator[F, ?], List[Path]]) {
      case (groupedFiles, file) =>
        extractors.filter(_.fileRelevant(file.toNioPath)).foldLeft(groupedFiles) {
          case (groupedFiles, orchestrator) =>
            groupedFiles.updated(orchestrator, file :: groupedFiles.getOrElse(orchestrator, Nil))
        }
    }.flatMap(map => Stream.fromIterator[F](map.iterator, 10))
  }

  private def buildOrchestratorMaven[F[_]: Async: Parallel](libInfoStorageDir: JPath,
                                                            mavenRepoUrl: String,
                                                            httpClient: Client[F]
                                                           ): Orchestrator[F, IdMaven] = {
    val buildTargetExtractor = new BuildTargetExtractorMaven[F]()

    val idConverter = IdConverterIonMaven()
    val coordinateConverter = CoordinateConverterIon(idConverter)
    val metaDataConverter = MetaDataConverterIon(coordinateConverter)

    val metaDataFetcherGit = new MetaDataFetcherGit(libInfoStorageDir, metaDataConverter)

    val metaDataFetcherMaven = MetaDataFetcherMaven(httpClient, mavenRepoUrl)
    val metaDataStore = MetaDataStoreGit(libInfoStorageDir, metaDataConverter)

    val calculator =
      new MetaDataCalculatorLocal(metaDataFetcherMaven, metaDataStore)

    val resolver = new DefaultResolver(metaDataFetcherGit, calculator, new NaiveResolutionModel())

    val libInfoFetcher = new LibInfoFetcherMaven(httpClient, mavenRepoUrl)

    val libInfoStore = new LibInfoStoreGit(libInfoStorageDir)
    Orchestrator(buildTargetExtractor, resolver, libInfoFetcher, libInfoStore)
  }

}