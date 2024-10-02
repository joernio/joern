package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.{PackageIdentifier, PackageMetadata, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.DirectDependency

import java.io.InputStream
import java.nio.file.{Path, Paths}
import scala.util.Try

/** The Type Information storage system stores a versions info file which lists all versions of a package. For each 
 * versioned package, there is metadata for this specific version and package which stores: direct dependencies 
 * (see DirectDependency class), transitive dependencies (see TransitiveDependency class), */
abstract class Fetcher extends AutoCloseable {
    protected final case class FetcherResult(path: ServerPath, data: InputStream)

    /** Much of fetching is building paths to resources and converting between different types of paths: paths that
     * generic fetching understands (e.g., path to version info or dependency files), paths that a specific fetcher
     * understands (e.g., git repo paths), and filesystem paths. To avoid mixing incompatible Path instances, instances
     * of ServerPath are given to abstract methods that deriving types should implement; these deriving types can use
     * ServerPaths as they are or convert between other fetcher-specific Path types. */
    protected final case class ServerPath(path: Path) {
        def getVersionInfoPath: ServerPath = {
            ServerPath(path.resolve("metadata").resolve("versions.ion"))
        }
        override def toString: String = path.toString
    }
    protected object ServerPath {
        def build(pid: PackageIdentifier, version: Option[Version] = None): ServerPath = {
            val packageDirPath = Paths.get(pid.platform.toString).resolve(pid.name)
            val finalPath = version match
                case None => packageDirPath
                case Some(version) => packageDirPath.resolve(version.toFetcherStr)
            ServerPath(finalPath)
        }
    }
    
    def fetchVersionsInfo(pid: PackageIdentifier): InputStream = {
        val infoFilePath = ServerPath.build(pid).getVersionInfoPath
        val downloadResults = downloadFiles(List(infoFilePath))
        downloadResults.head.data
    }
    
    /** Returns a map from type name -> an input stream to read data about this type. The type name keys are type
     * short names, the NAME property of a TYPE_DECL in a CPG. */
    def fetchAllTypeData(pid: PackageIdentifier, version: Version): Map[String, InputStream] = throw new NotImplementedError()
    
    def fetchDirectDependencies(pid: PackageIdentifier, version: Version): InputStream = throw new NotImplementedError()
    
    def fetchTransitiveDependencies(pid: PackageIdentifier, version: Version): InputStream = throw new NotImplementedError()
    
    protected def downloadFiles(path: List[ServerPath]): List[FetcherResult]
}
