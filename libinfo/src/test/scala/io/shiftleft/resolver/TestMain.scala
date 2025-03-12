package io.shiftleft.resolver

import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.shiftleft.resolver.api.*
import io.shiftleft.resolver.impl.*
import io.shiftleft.resolver.util.GitStorage
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.client.middleware.FollowRedirect
import org.http4s.ember.client.EmberClientBuilder

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path as JPath
import java.nio.file.Files as JFiles

object TestMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    Configurator.setRootLevel(Level.DEBUG)

    val idConverter = IdConverterIonMaven()
    val coordinateConverter = CoordinateConverterIon(idConverter)
    val metaDataConverter = MetaDataConverterIon(coordinateConverter)
    val repoDir = JPath.of("/tmp/libInfo")
    val fetcher = new MetaDataFetcherGit[IO, IdMaven](repoDir, metaDataConverter)

    val httpClientR = EmberClientBuilder.default[IO].build.map(FollowRedirect(10))
    httpClientR.use { httpClient =>
      val serverUrl = "https://repo1.maven.org/maven2"
      val calculator =
        new MetaDataCalculatorLocal(MetaDataFetcherMaven[IO](httpClient, serverUrl), MetaDataStoreGit(repoDir, metaDataConverter))

      val libInfoFetcher = new LibInfoFetcherMaven[IO](httpClient, serverUrl)

      val extractors = Vector(
        (BuildInfoExtractorMaven[IO](), new DefaultResolver[IO, IdMaven](fetcher, calculator, new NaiveResolutionModel()), libInfoFetcher),
        //SwiftBuildInfoExtractor[IO](null, null, null)
      )

      new TestMain(extractors).run()
    }


  }

}

type ExtractorTupleType[F[_], I <: Id] = (BuildInfoExtractor[F, I], Resolver[F, I], LibInfoFetcher[F, I])

class TestMain[I <: Id](extractors: Vector[ExtractorTupleType[IO, I]]) {

  def run(): IO[ExitCode] = {
    val stream = 
    associateBuildFiles(Files[IO].walk(Path("/tmp/testData")), extractors).flatMap {
      case ((extractor, resolver, libInfoFetcher), buildFiles) =>
      Stream.evalSeq(extractor.extractBuildTargets(buildFiles.map(_.toNioPath))).parEvalMapUnbounded {
        case buildTarget =>
          resolver.resolve(buildTarget.directDependencies).flatMap { case resolvedDeps =>
            IO.println(s"${buildTarget.id} $resolvedDeps") >>
              Stream.emits(resolvedDeps).through(libInfoFetcher.fetch2).evalMap { case (dep, libInfoOption) =>
                libInfoOption match {
                  case Some(libInfo) =>
                    val path = GitStorage.calcLibInfoPath(JPath.of("/tmp/testData/libInfo"), dep.id.value, dep.version)
                    IO.blocking(JFiles.createDirectories(path.getParent)) >>
                    IO.blocking(JFiles.writeString(path, libInfo.libInfo))
                  case None =>
                    IO.println(s"Failed to fetch lib info for $dep")
                }
              }.compile.drain
          }
      }
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

  private def associateBuildFiles[F[_]: Sync, I <: Id](fileStream: Stream[F, Path],
                                                               extractors: Vector[ExtractorTupleType[F, I]])
  : Stream[F, (ExtractorTupleType[F, I], List[Path])] = {
    fileStream.fold(Map.empty[ExtractorTupleType[F, I], List[Path]]) {
      case (groupedFiles, file) =>
        extractors.filter(_._1.fileRelevant(file.toNioPath)).foldLeft(groupedFiles) {
          case (groupedFiles, extractorTuple) =>
            groupedFiles.updated(extractorTuple, file :: groupedFiles.getOrElse(extractorTuple, Nil))
        }
    }.flatMap(map => Stream.fromIterator[F](map.iterator, 10))
  }
}
