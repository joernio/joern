package io.shiftleft.resolver.api

trait MetaDataCalculator[F[_], I <: Id] {
  def calculateMetaData(deps: Vector[Coordinate[I]]): F[Unit]
}
