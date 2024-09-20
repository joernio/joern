package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, Version}

case class Dependency(name: PackageIdentifier, version: Version)
case class SolvedDependency(name: PackageIdentifier, version: Version)
