package io.shiftleft.resolver.api

import java.nio.file.Path

trait BuildTargetExtractor[F[_], Output] {
  def fileRelevant(path: Path): Boolean

  def extractBuildTargets(buildFiles: List[Path]): F[Vector[Output]]
  
}

