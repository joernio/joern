package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, PackageMetadata, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.DirectDependency

import java.io.InputStream
import scala.util.Try

/** An interface for access to a Type Information storage system. The Type Information storage system stores
 * a version directory which lists all versions of a package. For each versioned package, there is metadata for this
 * specific version and package which stores: direct dependencies (see DirectDependency class), transitive dependencies
 * (see TransitiveDependency class), */
abstract class Fetcher {
    /** Package  */
    def fetchVersionDirectory(pid: PackageIdentifier): InputStream
    
    /** Returns a map from type name -> an input stream to read data about this type. The type name keys are type
     * short names, the NAME property of a TYPE_DECL in a CPG.
     * @throws */
    def fetchAllTypeData(pid: PackageIdentifier, version: Version): Map[String, InputStream]
    
    def fetchDirectDependencies(pid: PackageIdentifier, version: Version): InputStream
    
    def fetchTransitiveDependencies(pid: PackageIdentifier, version: Version): InputStream
    
    protected def downloadFile(path: Path): InputStream
}
