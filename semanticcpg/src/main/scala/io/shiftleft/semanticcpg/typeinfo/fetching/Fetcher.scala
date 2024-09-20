package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.Dependency

sealed trait FetchingError
case class NoPackageWithThatName(name: PackageIdentifier) extends FetchingError
case class NoPackageWithThatVersion(name: PackageIdentifier, version: Version) extends FetchingError

trait Fetcher:
  def getVersions(name: PackageIdentifier): Either[FetchingError, List[String]]
  def getPackage(name: PackageIdentifier, version: Version): Either[FetchingError, Package]
  def getDependencyInfo(name: PackageIdentifier, version: Version): Either[FetchingError, List[Dependency]]
