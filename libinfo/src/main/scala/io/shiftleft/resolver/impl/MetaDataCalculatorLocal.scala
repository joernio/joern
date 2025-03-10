package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.Async
import cats.syntax.all.*
import io.shiftleft.resolver.api.{Coordinate, Id, MetaDataCalculator, MetaDataFetcher, MetaDataStore}

class MetaDataCalculatorLocal[F[_]: {Async, Parallel}, I <: Id](metaDataFetcher: MetaDataFetcher[F, I],
                                                       metaDataStore: MetaDataStore[F, I]) extends MetaDataCalculator[F, I] {
  override def calculateMetaData(deps: Vector[Coordinate[I]]): F[Unit] = {
    metaDataFetcher.fetch(deps).flatMap { case (failedDeps, successfulMetaData) =>
      if (failedDeps.nonEmpty) {
        new RuntimeException(s"Failed to fetch dependencies $failedDeps")
      }
      metaDataStore.store(successfulMetaData)
    }
  }
}