package io.shiftleft.resolver.impl

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all.*
import com.amazon.ion.IonReader
import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.resolver.api.{Coordinate, Id, IdConverterIon, MetaData, MetaDataFetcher}
import io.shiftleft.resolver.util.GitStorage

import java.nio.file.{Files, Path}

class MetaDataFetcherGit[F[_]: Sync, I <: Id](repoDir: Path,
                                              metaDataConverter: MetaDataConverterIon[I]) extends MetaDataFetcher[F, I] {

  override def fetch(deps: Vector[Coordinate[I]]): F[(Vector[Coordinate[I]], Vector[MetaData[I]])] = {
    val pairsF =
      deps.map { dep =>
        val depMetaDataPairF =
          for {
            metaDataPath <- Sync[F].pure(GitStorage.calcMetaDataPath(repoDir, dep.id.value, dep.version))
            metaDataExists <- Sync[F].blocking(Files.exists(metaDataPath))
            metaDataOption <- if (metaDataExists) {
              readMetaData(metaDataPath).map(Some.apply)
            } else {
              Sync[F].pure(None)
            }
          } yield (dep, metaDataOption)
        depMetaDataPairF
      }.sequence
    
    pairsF.map { vector =>
      val (failedFetch, successfulFetch) = vector.partition(_._2.isEmpty)
      (failedFetch.map(_._1), successfulFetch.map(_._2.get))
    }
  }

  private def readMetaData(path: Path): F[MetaData[I]] = {
    for {
      metaDataRaw <- Sync[F].blocking(Files.readString(path))
      reader <- Sync[F].pure(IonReaderBuilder.standard().build(metaDataRaw))
      metaData <- Sync[F].pure(metaDataConverter.readMetaData(reader))
    } yield metaData
  }
}