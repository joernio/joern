package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.{PackageMetadata, TypeDecl, TypeInfoIonTextLoader, TypeMetadata}
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}
import io.shiftleft.semanticcpg.typeinfo.version.Version

trait TypeStorageLoader {

  /** Takes a version parser and an array of bytes to parse */
  def loadDirectDependencies(parseVersion: String => Version, bytes: Array[Byte]): List[DirectDependency]

  /** Takes a version parser and an array of bytes to parse */
  def loadTransitiveDependencies(parseVersion: String => Version, bytes: Array[Byte]): List[TransitiveDependency]

  /** Loads raw version strings, to be parsed by a deriver of Version */
  def loadMetadataVersions(bytes: Array[Byte]): List[String]

  /** TODO: might cut for now, intended for use later if building an index of (type names -> containing package names) */
  def loadMetadataTypeNames(bytes: Array[Byte]): List[TypeMetadata]

  /** TODO: will add when adding code to write data out to the github repo */
  def loadMetadata(bytes: Array[Byte]): PackageMetadata

  def loadTypeInfo(bytes: Array[Byte]): TypeDecl
}

object Loader extends TypeStorageLoader {
  override def loadDirectDependencies(parseVersion: String => Version, bytes: Array[Byte]): List[DirectDependency] =
    DirectDependencyIonTextLoader.loadFromBytes(parseVersion, bytes)

  override def loadTransitiveDependencies(
    parseVersion: String => Version,
    bytes: Array[Byte]
  ): List[TransitiveDependency] =
    TransitiveDependencyIonTextLoader.loadFromBytes(parseVersion, bytes)

  override def loadMetadata(bytes: Array[Byte]): PackageMetadata =
    throw new NotImplementedError()

  override def loadMetadataVersions(bytes: Array[Byte]): List[String] =
    MetadataIonTextLoader.loadRawVersionsFromBytes(bytes)

  override def loadMetadataTypeNames(bytes: Array[Byte]): List[TypeMetadata] =
    throw new NotImplementedError()

  override def loadTypeInfo(bytes: Array[Byte]): TypeDecl =
    TypeInfoIonTextLoader.loadFromBytes(bytes)
}
