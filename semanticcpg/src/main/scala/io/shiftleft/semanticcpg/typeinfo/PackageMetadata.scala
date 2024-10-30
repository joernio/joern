package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.version.Version
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}

/** Unused at the moment, will be used when writing details to be committed to the repo */
final case class PackageMetadata(versions: List[String] = Nil)
