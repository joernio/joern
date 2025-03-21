package io.shiftleft.resolver.api

case class BuildTarget[I <: Id](id: I, directDependencies: Vector[Coordinate[I]])
