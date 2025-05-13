package io.shiftleft.resolver.api

trait Resolver[F[_], I <: Id, Input] {
  def resolve(input: Input): F[Vector[Coordinate[I]]]
}
