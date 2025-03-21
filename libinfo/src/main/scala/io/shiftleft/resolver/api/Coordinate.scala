package io.shiftleft.resolver.api

case class Coordinate[I <: Id](id: I, version: String) {
  override def toString: String = {
    s"$id:$version"
  }
}
