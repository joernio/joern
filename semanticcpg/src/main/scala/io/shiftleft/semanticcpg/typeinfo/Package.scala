package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}

final case class Package(
                          id: PackageIdentifier,
                          version: Version,
                          lang: LanguagePlatform,
                          types: Map[String, TypeDecl],
                          directDependencies: List[DirectDependency],
                          transitiveDependencies: List[TransitiveDependency]
)
