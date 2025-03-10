package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all.*
import io.shiftleft.resolver.api.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.VectorMap

// Conflict resolution
// mvn - smallest distance to root wins
// ivy - latest revision wins
// gradle - highest version wins
class DefaultResolver[F[_]: {Sync, Parallel}, I <: Id](metaDataFetcher: MetaDataFetcher[F, I],
                                              metaDataCalculator: MetaDataCalculator[F, I],
                                              resolutionModel: ResolutionModel[F, I]) extends Resolver[F, I] {
  private given Logger[F] = Slf4jLogger.getLogger[F]()
  type CoordT = Coordinate[I]
  type MetaDataT = MetaData[I]

  override def resolve(directDeps: Vector[CoordT]): F[Vector[CoordT]] = {
    for {
      resolvedDepsMap <- resolve(VectorMap.empty, directDeps, "<root>")
      resolvedDeps = resolvedDepsMap.values.toVector
    } yield resolvedDeps
  }

  private def resolve(resolvedDeps: VectorMap[I, CoordT],
                      directDeps: Vector[CoordT],
                      libOwnerId: String): F[VectorMap[I, CoordT]] = {
    for {
      _ <- Logger[F].debug(s"Processing deps of $libOwnerId.\nDirectDeps: ${directDeps.mkString(",")}")
      (mergedResolvedDeps, newDeps) <- Sync[F].pure(mergeDeps(resolvedDeps, directDeps))
      _ <- Logger[F].debug(s"merged deps:\n${vToString(mergedResolvedDeps)}\nnew deps:\n${vToString(newDeps)}")
      depMetaDatas <- getMetaData(newDeps.values.toVector)
      resultingResolvedDeps <- resolveDepsOfDeps(mergedResolvedDeps, depMetaDatas)
    } yield resultingResolvedDeps
  }

  private def resolveDepsOfDeps(resolvedDeps: VectorMap[I, CoordT],
                                depMetaDatas: Vector[MetaDataT]): F[VectorMap[I, CoordT]] = {
    depMetaDatas.foldLeft(Sync[F].pure(resolvedDeps)) { case (resolvedDeps, depMetaData) =>
      resolvedDeps.flatMap(resolve(_, depMetaData.directDeps, depMetaData.coordinate.id.value))
    }
  }

  private def getMetaData(deps: Vector[CoordT]): F[Vector[MetaDataT]] = {
    for {
      (depsWithMissingMetaData, metaDatas) <- metaDataFetcher.fetch(deps)
      metaDatas <-
        if (depsWithMissingMetaData.isEmpty) {
          Sync[F].pure(metaDatas)
        } else {
          metaDataCalculator.calculateMetaData(depsWithMissingMetaData) >>
            metaDataFetcher.fetch(deps).map { case (missing, metaDatas) =>
              if (missing.nonEmpty) {
                throw new RuntimeException(s"Failed to calculate meta data for: $missing")
              } else {
                metaDatas
              }
            }
        }
    } yield metaDatas
  }

  private def mergeDeps(resolvedDeps: VectorMap[I, CoordT],
                        deps: Vector[CoordT],
                       ): (VectorMap[I, CoordT], VectorMap[I, CoordT]) = {
    val emptyNewDeps = VectorMap.empty[I, CoordT]
    deps.foldLeft((resolvedDeps, emptyNewDeps)) { case ((resolvedDeps, newDeps), dep) =>
      resolvedDeps.get(dep.id) match {
        case Some(existingDep) =>
          val updatedState = resolutionModel.calculateReplacement(existingDep, dep)
            .map(conflictResolvedDep => (resolvedDeps.updated(conflictResolvedDep.id, conflictResolvedDep),
              newDeps.updated(conflictResolvedDep.id, conflictResolvedDep)))
            .getOrElse((resolvedDeps, newDeps))
          updatedState
        case None =>
          (resolvedDeps.updated(dep.id, dep), newDeps.updated(dep.id, dep))
      }
    }
  }

  private def vToString(vectorMap: VectorMap[I, CoordT]): String = {
    vectorMap.map {
      case (str, dep) =>
        (str, dep.version)
    }.mkString("\n")
  }
}
