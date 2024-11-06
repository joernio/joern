package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, Eq, TransitiveDependency}
import io.shiftleft.semanticcpg.typeinfo.version.Version

import java.io.InputStream

/** parses as a DirectDependency and then ensures that all constraints are just version equal constraints, and maps to
  * TransitiveDependencies
  */
object TransitiveDependencyIonTextLoader {
  def loadFromStream(versionParser: String => Version, data: InputStream): List[TransitiveDependency] = {
    val directDeps = DirectDependencyIonTextLoader.loadFromStream(versionParser, data)
    validate(directDeps)
    directDeps.map { case DirectDependency(name, Eq(version)) =>
      TransitiveDependency(name, version)
    }
  }

  private def validate(transDepsAsDirectDeps: List[DirectDependency]): Unit = {
    transDepsAsDirectDeps.foreach {
      case DirectDependency(name, Eq(version)) => ()
      case other =>
        throw new Exception(
          s"Transitive Dependency, $other, has unresolved version constraint but should be a single version"
        )
    }
  }
}
