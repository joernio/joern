package io.shiftleft.resolver.impl

import io.shiftleft.resolver.api.{Coordinate, ResolutionModel}

class MavenResolutionModel[F[_]] extends ResolutionModel[F, IdMaven] {

  override def calculateReplacement(existingDep: Coordinate[IdMaven], newDep: Coordinate[IdMaven]): Option[Coordinate[IdMaven]] = ???

}
