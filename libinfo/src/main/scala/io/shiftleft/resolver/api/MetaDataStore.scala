package io.shiftleft.resolver.api

trait MetaDataStore[F[_], I <: Id] {
  def store(metaDatas: Vector[MetaData[I]]): F[Unit]
}
