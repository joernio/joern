package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM
import io.shiftleft.semanticcpg.typeinfo.dependencies.*
import io.shiftleft.semanticcpg.typeinfo.fetching.Fetcher
import io.shiftleft.semanticcpg.typeinfo.loading.{DirectDependencyIonTextLoader, MetadataIonTextLoader}
import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, PackageMetadata}
import io.shiftleft.semanticcpg.typeinfo.version.{SemVer2, Version}

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/**
 * TODO: Maybe just give Version class as type parameter
 * 
 * TODO: this is using semver2 for now, but maven uses semver1 and allows some non-sem-ver
 * versions: https://maven.apache.org/pom.html#Dependencies
 * 
 * TODO Resolution:
 * https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html
 * If multiple versions are allowed for a dependency, then the 
 * version of the library with the shortest dependency chain is used. */
object MavenDependencyResolver {
  def resolveDependencies(fetcher: Fetcher, pid: PackageIdentifier, version: Version): Future[List[TransitiveDependency]] = {
    val versionsMap: mutable.LinkedHashMap[String, List[SemVer2]] = mutable.LinkedHashMap()
    val constraintsMap: mutable.LinkedHashMap[String, SemVer2 => Boolean] = mutable.LinkedHashMap()
    
    val directDeps = getDirectDependencies(fetcher, pid, version).result(30.seconds)
    
  }
  
  private def processWorkList(worklist: Set[DirectDependency],
                              versionsMap: mutable.LinkedHashMap[String, List[SemVer2]],
                              constraintsMap: mutable.LinkedHashMap[String, SemVer2 => Boolean]): Set[DirectDependency] = {
    if (worklist.isEmpty) {
      return worklist
    }
    
    val nextDirectDep = worklist.head
    val nextWorklist = worklist.tail
    
    val name = nextDirectDep.name
    val versionConstraint = nextDirectDep.version
    
    // All of its available versions have already been added, and a previous constraint already added
    if (versionsMap.contains(name)) {
      val prevConstraint = constraintsMap.get(name).get
      val newConstraint = getConstraintEvaluator(versionConstraint)
      val finalConstraint: SemVer2 => Boolean = other => prevConstraint(other) && newConstraint(other)
      constraintsMap.put(name, finalConstraint)
      return nextWorklist
    }
    
    
    
    nextWorklist
  } 
  
  private def buildWorkList(directDeps: Future[List[DirectDependency]]): Future[Set[DirectDependency]] = {
    directDeps.flatMap(deps => Future(deps.toSet))
  }
  
  private def buildWorkList(worklist: Set[DirectDependency], newDeps: List[Future[List[DirectDependency]]]): Future[Set[DirectDependency]] = {
    Future.sequence(newDeps).flatMap(newDeps => Future(worklist.union(newDeps.flatten.toSet)))
  }
  
  /** Returns a function that checks for another direct dependency if the version for the other direct dependency is 
   * satisfied by the constraints of directDependency */
  private def getConstraintEvaluator(versionConstraint: VersionConstraint): SemVer2 => Boolean = { otherVersion =>
    versionConstraint match
      case Eq(str) => str.equals(otherVersion)
      case Any() => true
      case Gt(Eq(str)) => otherVersion.compare(str) > 0
      case Gte(Eq(str)) => otherVersion.compare(str) >= 0
      case Lt(Eq(str)) => otherVersion.compare(str) < 0
      case Lte(Eq(str)) => otherVersion.compare(str) <= 0
      case Not(constraint) => !getConstraintEvaluator(constraint)(otherVersion)
      case And(left, right) => getConstraintEvaluator(left)(otherVersion) && getConstraintEvaluator(right)(otherVersion)
      case Or(left, right) => getConstraintEvaluator(left)(otherVersion) || getConstraintEvaluator(right)(otherVersion)
  }
  
  private def getDirectDependencies(fetcher: Fetcher, pid: PackageIdentifier, version: Version): Future[List[DirectDependency]] = {
    fetcher.fetchDirectDependencies(pid, version).flatMap { bytes => 
      Future {
        val deps = DirectDependencyIonTextLoader(SemVer2.apply).loadFromBytes(bytes)
      }
    }
  }
  
  private def getAvailableVersions(fetcher: Fetcher, packageName: String): Future[List[SemVer2]] = {
    val pid = PackageIdentifier(JVM, packageName)
    fetcher.fetchMetaData(pid).flatMap { bytes =>
      Future {
        MetadataIonTextLoader.loadFromBytes(bytes).versions.map(SemVer2.apply)
      }
    }
  }
}
