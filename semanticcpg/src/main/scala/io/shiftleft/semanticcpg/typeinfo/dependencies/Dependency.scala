package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, Version}

/** DirectDependency represents a dependency as specified by the package (but with version constraints
 * translated to our VersionConstrant format. */
final case class DirectDependency(name: PackageIdentifier, version: ConstrainedVersion)

/** TransitiveDependency represents the dependency whose type info will be used. This represents a dependency
 * which has its version constraints already resolved to this specific version. */
final case class TransitiveDependency(name: PackageIdentifier, version: Version)
