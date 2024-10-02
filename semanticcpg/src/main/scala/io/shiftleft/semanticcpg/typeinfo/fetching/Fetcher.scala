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
    def fetchVersionsInfo(pid: PackageIdentifier): InputStream = {
        val versionsInfoPath = appendVersionInfoFile(buildServerPath(pid))
        val downloadResults = downloadFiles(List(versionsInfoPath))
        downloadResults.head.data
    }
    
    /** Returns a map from type name -> an input stream to read data about this type. The type name keys are type
     * short names, the NAME property of a TYPE_DECL in a CPG.
     * @throws */
    def fetchAllTypeData(pid: PackageIdentifier, version: Version): Map[String, InputStream] = throw new NotImplementedError()
    
    def fetchDirectDependencies(pid: PackageIdentifier, version: Version): InputStream = throw new NotImplementedError()
    
    def fetchTransitiveDependencies(pid: PackageIdentifier, version: Version): InputStream = throw new NotImplementedError()
    
    protected final case class DownloadResult(path: Path, data: InputStream)
    
    protected def downloadFiles(path: List[Path]): List[DownloadResult]

    override def close(): Unit = ()
    
    private def buildServerPath(pid: PackageIdentifier, version: Option[Version] = None): Path = {
        val packageDirPath = Paths.get(pid.platform.toString).resolve(pid.name)
        version match
            case None => packageDirPath
            case Some(version) => packageDirPath.resolve(version.toFetcherStr)
    }
    
    private def appendVersionInfoFile(packageDirPath: Path): Path = {
        packageDirPath.resolve("metadata").resolve("versions.ion")
    }
}
