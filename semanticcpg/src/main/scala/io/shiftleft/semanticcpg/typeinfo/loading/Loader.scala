package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.{PackageMetadata, TypeDecl, TypeInfoIonTextLoader}
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}
import io.shiftleft.semanticcpg.typeinfo.version.Version

import java.io.InputStream

trait TypeStorageLoader {

  /** Takes a version parser and an array of bytes to parse */
  def loadDirectDependencies(parseVersion: String => Version, inputStream: InputStream): List[DirectDependency]

  /** Takes a version parser and an array of bytes to parse */
  def loadTransitiveDependencies(parseVersion: String => Version, inputStream: InputStream): List[TransitiveDependency]

  /** Loads raw version strings, to be parsed by a deriver of Version */
  def loadMetadataVersions(inputStream: InputStream): List[String]

  /** TODO: will add when adding code to write data out to the github repo */
  def loadMetadata(inputStream: InputStream): PackageMetadata

  def loadTypeInfo(inputStream: InputStream): TypeDecl
}

/** This loader loads Ion Text data from the InputStream */
object Loader extends TypeStorageLoader {
  override def loadDirectDependencies(parseVersion: String => Version, inputStream: InputStream): List[DirectDependency] =
    DirectDependencyIonTextLoader.loadFromStream(parseVersion, inputStream)

  override def loadTransitiveDependencies(
    parseVersion: String => Version,
    inputStream: InputStream
  ): List[TransitiveDependency] =
    TransitiveDependencyIonTextLoader.loadFromStream(parseVersion, inputStream)

  override def loadMetadata(inputStream: InputStream): PackageMetadata =
    throw new NotImplementedError()

  override def loadMetadataVersions(inputStream: InputStream): List[String] =
    MetadataIonTextLoader.loadRawVersionsFromStream(inputStream)

  override def loadTypeInfo(inputStream: InputStream): TypeDecl =
    TypeInfoIonTextLoader.loadFromStream(inputStream)
}
