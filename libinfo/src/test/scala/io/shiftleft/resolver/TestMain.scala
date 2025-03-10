package io.shiftleft.resolver

import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.shiftleft.resolver.api.*
import io.shiftleft.resolver.impl.*
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.config.Configurator
import org.http4s.client.middleware.FollowRedirect
import org.http4s.ember.client.EmberClientBuilder

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path as JPath

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

      val extractors = Vector(
        (BuildInfoExtractorMaven[IO](), new DefaultResolver[IO, IdMaven](fetcher, calculator, new NaiveResolutionModel())),
        //SwiftBuildInfoExtractor[IO](null, null, null)
      )

      new TestMain(extractors).run()
    }


  }

}

class TestMain[I <: Id](extractors: Vector[(BuildInfoExtractor[IO, I], Resolver[IO, I])],
               ) {

  def run(): IO[ExitCode] = {
    val stream = 
    associateBuildFiles(Files[IO].walk(Path("/tmp/testData")), extractors).flatMap { case ((extractor, resolver), buildFiles) =>
      Stream.evalSeq(extractor.extractBuildTargets(buildFiles.map(_.toNioPath))).parEvalMapUnbounded {
        case buildTarget =>
          resolver.resolve(buildTarget.directDependencies).flatMap { case resolvedDeps =>
            val x = IO.println(s"${buildTarget.id} $resolvedDeps")
            x
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

  private def resolveBuildTargets[I <: Id](buildTargets: Vector[BuildTarget[I]],
                                           resolver: Resolver[IO, I]
                                          ): IO[Vector[(BuildTarget[I], Vector[Coordinate[I]])]] = {
    buildTargets.map { buildTarget =>
      resolver.resolve(buildTarget.directDependencies).map(resolvedDeps => (buildTarget, resolvedDeps))
    }.parSequence
  }

  private def associateBuildFiles[F[_]: Sync, I <: Id](fileStream: Stream[F, Path],
                                                               extractors: Vector[(BuildInfoExtractor[F, I], Resolver[F, I])])
  : Stream[F, ((BuildInfoExtractor[F, I], Resolver[F, I]), List[Path])] = {
    fileStream.fold(Map.empty[(BuildInfoExtractor[F, I], Resolver[F, I]), List[Path]]) {
      case (groupedFiles, file) =>
        extractors.filter(_._1.fileRelevant(file.toNioPath)).foldLeft(groupedFiles) { case (groupedFiles, extractorResolverPair) =>
          groupedFiles.updated(extractorResolverPair, file :: groupedFiles.getOrElse(extractorResolverPair, Nil))
        }
    }.flatMap(map => Stream.fromIterator[F](map.iterator, 10))
  }
}
