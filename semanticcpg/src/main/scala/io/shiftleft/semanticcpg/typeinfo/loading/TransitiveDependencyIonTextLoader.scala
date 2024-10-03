package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, Eq, TransitiveDependency}
import io.shiftleft.semanticcpg.typeinfo.version.Version

/** parses as a DirectDependency and then ensures that all constraints are just version equal constraints,
 * and maps to TransitiveDependencies */
class TransitiveDependencyIonTextLoader(versionParser: String => Version) extends BytesLoader[List[TransitiveDependency]] {
  def loadFromBytes(data: Array[Byte]): List[TransitiveDependency] = {
    val directDeps = DirectDependencyIonTextLoader(versionParser).loadFromBytes(data)
    validate(directDeps)
    directDeps.map {
      case DirectDependency(name, Eq(version)) => TransitiveDependency(name, version)
    }
  }

  private def validate(transDepsAsDirectDeps: List[DirectDependency]): Unit = {
    transDepsAsDirectDeps.foreach {
      case DirectDependency(name, Eq(version)) => ()
      case other => throw new Exception(s"Transitive Dependency, $other, has unresolved version constraint but should be a single version")
    }
  }
}
