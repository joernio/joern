package io.shiftleft.resolver.api

trait MetaDataFetcher[F[_], I <: Id] {

  def fetch(deps: Vector[Coordinate[I]]): F[(Vector[Coordinate[I]], Vector[MetaData[I]])]
}
