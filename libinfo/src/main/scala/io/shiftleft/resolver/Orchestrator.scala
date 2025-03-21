package io.shiftleft.resolver

import cats.effect.Async
import io.shiftleft.resolver.api.{BuildTargetExtractor, Coordinate, Id, LibInfoFetcher, LibInfoHandle, LibInfoStore, Resolver}
import cats.syntax.all.*

import java.nio.file.Path
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class Orchestrator[F[_]: Async, I <: Id](extractor: BuildTargetExtractor[F, I],
                                         resolver: Resolver[F, I],
                                         libInfoFetcher: LibInfoFetcher[F, I],
                                         libInfoStore: LibInfoStore[F]
                                        ) {
  private given Logger[F] = Slf4jLogger.getLogger[F]()

  def fileRelevant(path: Path): Boolean = {
    extractor.fileRelevant(path)
  }

  def run(buildFiles: List[Path]): Stream[F, Unit] = {
    Stream.evalSeq(extractor.extractBuildTargets(buildFiles)).parEvalMapUnbounded { buildTarget =>
        resolver.resolve(buildTarget.directDependencies).flatMap { resolvedDeps =>
          Logger[F].debug(s"${buildTarget.id} $resolvedDeps") >>
            Stream.emits(resolvedDeps).through(libInfoFetcher.fetch).evalMap { case (dep, libInfoOption) =>
              libInfoOption match {
                case Some(libInfo) =>
                  libInfoStore.store(dep, libInfo)
                case None =>
                  Logger[F].warn(s"Failed to fetch lib info for $dep")
              }
            }.compile.drain
        }
    }
  }
}
