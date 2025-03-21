package io.shiftleft.resolver.api

import fs2.Stream

case class LibInfoHandle(libInfo: String) {

}

trait LibInfoFetcher[F[_], I <: Id] {
  def fetch(deps: Stream[F, Coordinate[I]]): Stream[F, (Coordinate[I], Option[LibInfoHandle])]
}
