package io.shiftleft.resolver.impl

import io.shiftleft.resolver.api.{Coordinate, Id, ResolutionModel}

class NaiveResolutionModel[F[_], I <: Id] extends ResolutionModel[F, I] {
  override def calculateReplacement(existingDep: Coordinate[I], newDep: Coordinate[I]): Option[Coordinate[I]] = {
    if (existingDep.version < newDep.version)  {
      Some(newDep)
    } else {
      None
    }
  }
}
