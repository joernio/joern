package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.version.Version
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}

final case class TypeMetadata(version: String = "", typeNames: List[String] = Nil)
final case class PackageMetadata(versions: List[String] = Nil, perVersionTypeInfo: List[TypeMetadata] = Nil)
