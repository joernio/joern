package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, Version}

final case class Dependency(name: PackageIdentifier, version: Constraint)
final case class ResolvedDependency(name: PackageIdentifier, version: Version)
