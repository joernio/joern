package io.shiftleft.resolver.api

import java.nio.file.Path

case class Coordinate[I <: Id](id: I, version: String) {
  override def toString: String = {
    s"$id:$version"
  }
}

case class BuildTarget[I <: Id](id: I, directDependencies: Vector[Coordinate[I]])

trait BuildInfoExtractor[F[_], I <: Id] {
  def fileRelevant(path: Path): Boolean

  def extractBuildTargets(buildFiles: List[Path]): F[Vector[BuildTarget[I]]]
  
}

