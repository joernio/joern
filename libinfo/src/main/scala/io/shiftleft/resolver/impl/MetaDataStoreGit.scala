package io.shiftleft.resolver.impl

import cats.effect.{Resource, Sync}
import cats.syntax.all.*
import com.amazon.ion.{IonType, IonWriter}
import com.amazon.ion.system.IonTextWriterBuilder
import io.shiftleft.resolver.api.{Id, IdConverterIon, MetaData, MetaDataStore}
import io.shiftleft.resolver.util.GitStorage

import java.io.OutputStream
import java.nio.file.{Files, Path}

class MetaDataStoreGit[F[_]: Sync, I <: Id](repoDir: Path, metaDataConverterIon: MetaDataConverterIon[I]) extends MetaDataStore[F, I] {

  override def store(metaDatas: Vector[MetaData[I]]): F[Unit] = {
    metaDatas.map { metaData =>
      for {
        metaDataPath <- Sync[F].pure(GitStorage.calcMetaDataPath(repoDir, metaData.coordinate.id.value, metaData.coordinate.version))
        _ <- Sync[F].blocking(Files.createDirectories(metaDataPath.getParent))
        _ <- writeMetaData(metaDataPath, metaData)
      } yield ()
    }.sequence.as(())
  }
  
  private def writeMetaData(path: Path, metaData: MetaData[I]): F[Unit] = {
    val outStreamR =
      Resource.make(Sync[F].blocking(Files.newOutputStream(path)))(outStream => Sync[F].blocking(outStream.close))

    outStreamR.use { outStream =>
      writeMetaData(outStream, metaData)
    }
  }

  private def writeMetaData(outStream: OutputStream, metaData: MetaData[I]): F[Unit] = {
    makeWriter(outStream).use { writer =>
      Sync[F].blocking {
        metaDataConverterIon.writeMetaData(writer, metaData)
      }
    }
  }

  private def makeWriter(outStream: OutputStream): Resource[F, IonWriter] = {
    Resource.make(Sync[F].blocking(IonTextWriterBuilder.pretty().build(outStream)))(writer => Sync[F].blocking(writer.close))
  }
}
