package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.version.Version

/** DirectDependency represents a dependency as specified by the package (but with version constraints translated to our
  * VersionConstraint format. TODO: these name strings depend on the language and package manager
  */
final case class DirectDependency(name: String, version: VersionConstraint)

/** TransitiveDependency represents the dependency whose type info will be used. This represents a dependency which has
  * its version constraints already resolved to this specific version.
  */
final case class TransitiveDependency(name: String, version: Version)
