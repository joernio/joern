package io.shiftleft.resolver.impl

import cats.effect.Sync
import io.shiftleft.resolver.api.BuildTargetExtractor

import java.nio.file.Path

class BuildTargetExtractorMavenOfficial[F[_]: Sync] extends BuildTargetExtractor[F, Path] {

  def fileRelevant(path: Path): Boolean = {
    path.endsWith("pom.xml")
  }

  def extractBuildTargets(buildFiles: List[Path]): F[Vector[Path]] = {
    // TODO filter for main pom file
    Sync[F].pure(buildFiles.toVector)
    //Sync[F].delay(buildFiles.filter(_ == Path.of("pom.xml")).toVector)
  }
}
