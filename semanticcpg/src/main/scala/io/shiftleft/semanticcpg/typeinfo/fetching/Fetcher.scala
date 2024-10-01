package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.Dependency
import scala.util.Try

trait Fetcher {
    def getVersions(pid: PackageIdentifier): Try[List[String]]
    def getPackage(pid: PackageIdentifier, version: Version): Try[Package]
    def getDependencyInfo(pid: PackageIdentifier, version: Version): Try[List[Dependency]]
}
