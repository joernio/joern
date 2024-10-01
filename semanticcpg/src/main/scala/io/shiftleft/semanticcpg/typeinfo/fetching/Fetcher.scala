package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, PackageMetadata, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.DirectDependency
import scala.util.Try

trait Fetcher {
    def getVersions(pid: PackageIdentifier): Try[List[String]]
    def getPackage(pid: PackageIdentifier, version: Version): Try[PackageMetadata]
    def getDependencyInfo(pid: PackageIdentifier, version: Version): Try[List[DirectDependency]]
}
