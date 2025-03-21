package io.shiftleft.resolver.api

trait LibInfoStore[F[_]] {
  def store(dep: Coordinate[?], libInfo: LibInfoHandle): F[Unit]
}
