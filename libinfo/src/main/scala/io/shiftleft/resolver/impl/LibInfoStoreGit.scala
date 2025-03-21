package io.shiftleft.resolver.impl

import cats.effect.Sync
import cats.syntax.all.*
import io.shiftleft.resolver.api.{Coordinate, LibInfoHandle, LibInfoStore}
import io.shiftleft.resolver.util.GitStorage

import java.nio.file.{Files, Path}

class LibInfoStoreGit[F[_]: Sync](storageDir: Path) extends LibInfoStore[F] {
  override def store(dep: Coordinate[?], libInfo: LibInfoHandle): F[Unit] = {
    val path = GitStorage.calcLibInfoPath(storageDir, dep.id.value, dep.version)
    Sync[F].blocking(Files.createDirectories(path.getParent)) >>
      Sync[F].blocking(Files.writeString(path, libInfo.libInfo))
  }
}
