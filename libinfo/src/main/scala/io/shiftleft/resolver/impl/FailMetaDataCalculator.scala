package io.shiftleft.resolver.impl

import io.shiftleft.resolver.api.{Coordinate, Id, MetaDataCalculator}

class FailMetaDataCalculator[F[_], I <: Id] extends MetaDataCalculator[F, I] {

  override def calculateMetaData(deps: Vector[Coordinate[I]]): F[Unit] = {
    throw new RuntimeException(s"Failed to get meta data for $deps")
  }
}
