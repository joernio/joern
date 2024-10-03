package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.version.Version
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}

final case class PackageMetadata(
                                  id: PackageIdentifier,
                                  version: Version,
                                  platform: LanguagePlatform,
                                  directDependencies: List[DirectDependency],
                                  transitiveDependencies: List[TransitiveDependency]
)
