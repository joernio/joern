package io.shiftleft.resolver.api

trait Resolver[F[_], I <: Id] {
  def resolve(directDeps: Vector[Coordinate[I]]): F[Vector[Coordinate[I]]]
}
