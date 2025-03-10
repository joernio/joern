package io.shiftleft.resolver.api

object MetaData {
  def apply[I <: Id](id: I, version: String, directDeps: Vector[Coordinate[I]]) = {
    new MetaData(id, version, directDeps)
  }
}

case class MetaData[I <: Id](coordinate: Coordinate[I], directDeps: Vector[Coordinate[I]]) {
  def this(id: I, version: String, directDeps: Vector[Coordinate[I]]) = {
    this(Coordinate(id, version), directDeps)
  }
}
