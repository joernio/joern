package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.Sync
import io.shiftleft.resolver.api.{BuildInfoExtractor, BuildTarget, Coordinate, MetaDataCalculator, MetaDataFetcher, ResolutionModel}

import java.nio.file.Path

class BuildInfoExtractorSwift[F[_]: Sync: Parallel] extends BuildInfoExtractor[F, IdGeneric] {

  override def fileRelevant(path: Path): Boolean = ???

  override def extractBuildTargets(buildFiles: List[Path]): F[Vector[BuildTarget[IdGeneric]]] = ???
}
