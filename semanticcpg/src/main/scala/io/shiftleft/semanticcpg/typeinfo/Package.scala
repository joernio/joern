package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.dependencies.{Dependency, ResolvedDependency}

final case class Package(id: PackageIdentifier, 
                         version: Version,
                         lang: LanguageFrontend,
                         types: Map[String, TypeDecl],
                         rawDependencies: List[Dependency],
                         resolvedDependencies: List[ResolvedDependency])
