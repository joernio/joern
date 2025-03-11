package io.shiftleft.resolver.api

case class LibInfoHandle(libInfo: String) {

}

trait LibInfoFetcher[F[_], I <: Id] {
  def fetch(deps: Vector[Coordinate[I]]): F[(Vector[Coordinate[I]], Vector[LibInfoHandle])]
}
