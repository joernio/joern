package io.shiftleft.resolver.api

trait ResolutionModel[F[_], I <: Id] {
  def calculateReplacement(existingDep: Coordinate[I], newDep: Coordinate[I]): Option[Coordinate[I]]
}
