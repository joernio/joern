package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM
import io.shiftleft.semanticcpg.typeinfo.dependencies.*
import io.shiftleft.semanticcpg.typeinfo.fetching.Fetcher
import io.shiftleft.semanticcpg.typeinfo.loading.Loader
import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, PackageMetadata}
import io.shiftleft.semanticcpg.typeinfo.version.{SemVer2, Version}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** TODO: Maybe just give Version class as type parameter
  *
  * TODO: this is using semver2 for now, but maven uses semver1 and allows some non-sem-ver versions:
  * https://maven.apache.org/pom.html#Dependencies
  *
  * TODO Resolution: https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html If multiple
  * versions are allowed for a dependency, then the version of the library with the shortest dependency chain is used.
  */
object MavenDependencyResolver {
  private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

  private class CurrentChoices private (val choices: List[(String, SemVer2)]) {
    def this() = this(Nil)

    def push(packageName: String, pickedVersion: SemVer2): CurrentChoices = {
      CurrentChoices(choices :+ (packageName, pickedVersion))
    }

    def pop(): CurrentChoices = {
      if (choices.isEmpty)
      then this
      else CurrentChoices(choices.dropRight(1))
    }

    def toList: List[(String, SemVer2)] = choices

    def toTransitiveDependencies: List[TransitiveDependency] = {
      choices.map((packageName, version) => TransitiveDependency(packageName, version))
    }

    override def toString: String = choices.mkString(",")
  }

  private class ConstraintMap private (val map: Map[String, VersionConstraint]) {
    def this() = this(Map())

    def this(topLevelPackageName: String, constraint: VersionConstraint) = {
      this(Map.from(List((topLevelPackageName, constraint))))
    }

    def merge(other: ConstraintMap): ConstraintMap = {
      val mergedConstraints = other.map.foldLeft(map) { case (accMap, (packageName, versionConstraint)) =>
        if (accMap.contains(packageName)) {
          val existingConstraint = accMap(packageName)
          val newConstraint      = And(existingConstraint, versionConstraint)
          accMap.updated(packageName, newConstraint)
        } else {
          accMap.updated(packageName, versionConstraint)
        }
      }
      ConstraintMap(mergedConstraints)
    }

    def addConstraint(packageName: String, constraint: VersionConstraint): ConstraintMap = {
      ConstraintMap(map.updated(packageName, constraint))
    }

    def checkAllAllowed(choices: CurrentChoices): Boolean = {
      choices.toList.foldLeft(true) { case (acc, (packageName, versionPicked)) =>
        acc && checkVersionAllowed(packageName, versionPicked)
      }
    }

    def checkVersionAllowed(packageName: String, concreteVersion: SemVer2): Boolean = {
      map.get(packageName) match
        case Some(constraint) => getConstraintEvaluator(constraint)(concreteVersion)
        case None             => true
    }

    override def toString: String = map.mkString("->")
  }

  private class CurrentConstraints private (constraints: List[ConstraintMap]) {
    def this() = this(Nil)

    def collectAll: ConstraintMap = {
      if (constraints.isEmpty)
      then ConstraintMap()
      else constraints.reduce((x, y) => x.merge(y))
    }

    def push(packageName: String, versionConstraint: VersionConstraint): CurrentConstraints = {
      if (constraints.isEmpty) {
        val newConstraints = ConstraintMap(packageName, versionConstraint)
        CurrentConstraints(constraints :+ newConstraints)
      } else {
        val prevConstraints = constraints.last
        val newConstraints  = prevConstraints.addConstraint(packageName, versionConstraint)
        CurrentConstraints(constraints :+ newConstraints)
      }
    }

    def push(directDeps: List[DirectDependency]): CurrentConstraints = {
      val prevConstraints = if (constraints.isEmpty) then ConstraintMap() else constraints.last
      val newConstraints = directDeps.foldLeft(prevConstraints)((accMap, directDep) =>
        accMap.addConstraint(directDep.name, directDep.version)
      )
      CurrentConstraints(constraints :+ newConstraints)
    }

    def pop(): CurrentConstraints = {
      if (constraints.isEmpty)
      then this
      else CurrentConstraints(constraints.dropRight(1))
    }

    override def toString: String = constraints.mkString(";")
  }

  private class ResolutionCandidates private (val map: Map[String, Map[SemVer2, List[DirectDependency]]]) {
    def this() = this(Map())

    def mergeWithLoadedDeps(
      deps: Map[(PackageIdentifier, Version), List[DirectDependency]]
    ): (ResolutionCandidates, Boolean) = {
      val newMap = deps.foldLeft(map)((map, data) => {
        val pid        = data._1._1
        val semVer2    = data._1._2.asInstanceOf[SemVer2]
        val directDeps = data._2
        val versToDeps = map.getOrElse(pid.name, Map())
        if (versToDeps.contains(semVer2)) {
          map
        } else {
          map.updated(pid.name, versToDeps.updated(semVer2, directDeps))
        }
      })
      if (newMap == map)
      then (this, false)
      else (ResolutionCandidates(newMap), true)
    }

    def headOption: Option[(String, Map[SemVer2, List[DirectDependency]])] = map.headOption

    def tail: ResolutionCandidates = ResolutionCandidates(map.tail)

    override def toString: String = map.mkString("->")
  }

  def resolveDependencies(
    fetcher: Fetcher,
    pid: PackageIdentifier,
    version: SemVer2
  ): Future[List[TransitiveDependency]] = {
    val candidates = downloadAllTransitiveDependencies(fetcher, List(pid.name))
    candidates.foreach(candidates => logger.debug(s"Maven dependency resolution candidates are: $candidates"))
    candidates.map(pickAllVersions(_).toTransitiveDependencies)
  }

  private def pickAllVersions(
    candidates: ResolutionCandidates,
    constraintStack: CurrentConstraints = CurrentConstraints(),
    pickedVersions: CurrentChoices = CurrentChoices()
  ): CurrentChoices = {
    candidates.headOption match
      case None => pickedVersions
      case Some((packageName, versionsToConstraints)) => {
        val constraints        = constraintStack.collectAll
        val descendingVersions = versionsToConstraints.keys.toList.sortWith((x, y) => x.compare(y) > 0)
        pickPackageVersion(
          packageName,
          descendingVersions,
          versionsToConstraints,
          constraints,
          candidates.tail,
          constraintStack,
          pickedVersions
        )
      }
  }

  @tailrec
  private def pickPackageVersion(
    packageName: String,
    descendingVersions: List[SemVer2],
    versionsToConstraints: Map[SemVer2, List[DirectDependency]],
    constraintsTilNow: ConstraintMap,
    candidates: ResolutionCandidates,
    constraintStack: CurrentConstraints,
    pickedVersions: CurrentChoices
  ): CurrentChoices = {
    descendingVersions.headOption match
      case Some(nextHighestVersion) if constraintsTilNow.checkVersionAllowed(packageName, nextHighestVersion) => {
        val newChoices     = pickedVersions.push(packageName, nextHighestVersion)
        val newConstraints = constraintStack.push(versionsToConstraints(nextHighestVersion))
        pickAllVersions(candidates, newConstraints, newChoices)
      }
      case Some(unsatVersion) => {
        pickPackageVersion(
          packageName,
          descendingVersions.tail,
          versionsToConstraints,
          constraintsTilNow,
          candidates,
          constraintStack,
          pickedVersions
        )
      }
      case None => {
        // Unsatisfiable version constraints
        val errMsg = s"""Couldn't satisfy dependency version constraints for $packageName.
             |Picked versions so far were: $pickedVersions
             |Constraints so far were: $constraintStack""".stripMargin
        logger.error(errMsg)
        throw new RuntimeException(errMsg)
      }
  }

  private def downloadAllTransitiveDependencies(
    fetcher: Fetcher,
    packageNames: List[String],
    candidates: ResolutionCandidates = ResolutionCandidates()
  ): Future[ResolutionCandidates] = {
    downloadMetadata(fetcher, packageNames)
      .flatMap(downloadDirectDependencyInfo(fetcher, _))
      .map(listDirectDependencies)
      .flatMap { loadedDeps =>
        val (newCandidates, changed) = candidates.mergeWithLoadedDeps(loadedDeps)
        if (changed) {
          val nextToDownload = listNextToDownload(loadedDeps)
          downloadAllTransitiveDependencies(fetcher, nextToDownload, newCandidates)
        } else {
          Future(candidates)
        }
      }
  }

  private def downloadMetadata(
    fetcher: Fetcher,
    packageNames: List[String]
  ): Future[Map[PackageIdentifier, Array[Byte]]] = {
    val pids      = packageNames.map(PackageIdentifier(JVM, _))
    val metadatas = fetcher.fetchMetaData(pids)
    metadatas
  }

  private def downloadDirectDependencyInfo(
    fetcher: Fetcher,
    metadatas: Map[PackageIdentifier, Array[Byte]]
  ): Future[Map[(PackageIdentifier, Version), Array[Byte]]] = {
    val neededVersions = listNeededVersions(metadatas)
    val directDeps     = fetcher.fetchDirectDependencies(neededVersions)
    directDeps
  }

  private def listDirectDependencies(
    deps: Map[(PackageIdentifier, Version), Array[Byte]]
  ): Map[(PackageIdentifier, Version), List[DirectDependency]] = {
    deps.map((pidAndVersion, directDepBytes) => {
      val directDeps = Loader.loadDirectDependencies(SemVer2.apply, directDepBytes)
      (pidAndVersion, directDeps)
    })
  }

  private def listNextToDownload(deps: Map[(PackageIdentifier, Version), List[DirectDependency]]): List[String] = {
    deps.flatMap(data => data._2.map(_.name)).toList
  }

  @tailrec
  private def listNeededVersions(
    metadatas: Map[PackageIdentifier, Array[Byte]],
    versions: List[(PackageIdentifier, SemVer2)] = Nil
  ): List[(PackageIdentifier, SemVer2)] = {
    metadatas.headOption match
      case None => versions
      case Some((pid, metadataBytes)) => {
        val thisPackageAvailVersions = Loader
          .loadMetadataVersions(metadataBytes)
          .map(SemVer2.apply)
          .map(semver => (pid, semver))
        listNeededVersions(metadatas.tail, thisPackageAvailVersions ++ versions)
      }
  }

  /** Returns a function that checks for another direct dependency if the version for the other direct dependency is
    * satisfied by the constraints of directDependency
    */
  private def getConstraintEvaluator(versionConstraint: VersionConstraint): SemVer2 => Boolean = { otherVersion =>
    versionConstraint match
      case Eq(str)          => str.equals(otherVersion)
      case Any()            => true
      case Gt(Eq(str))      => otherVersion.compare(str) > 0
      case Gte(Eq(str))     => otherVersion.compare(str) >= 0
      case Lt(Eq(str))      => otherVersion.compare(str) < 0
      case Lte(Eq(str))     => otherVersion.compare(str) <= 0
      case Not(constraint)  => !getConstraintEvaluator(constraint)(otherVersion)
      case And(left, right) => getConstraintEvaluator(left)(otherVersion) && getConstraintEvaluator(right)(otherVersion)
      case Or(left, right)  => getConstraintEvaluator(left)(otherVersion) || getConstraintEvaluator(right)(otherVersion)
  }
}
