package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.{PackageMetadata, TypeDecl, TypeInfoIonTextLoader, TypeMetadata}
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}
import io.shiftleft.semanticcpg.typeinfo.version.Version

trait TypeStorageLoader {
  /** Takes a version parser and an array of bytes to parse */
  val loadDirectDependencies: (String => Version) => Array[Byte] => List[DirectDependency]

  /** Takes a version parser and an array of bytes to parse */
  val loadTransitiveDependencies: (String => Version) => Array[Byte] => List[TransitiveDependency]
  
  /** Loads raw version strings, to be parsed by a deriver of Version */
  val loadMetadataVersions: Array[Byte] => List[String]
  
  /** TODO: might cut */
  val loadMetadataTypeNames: Array[Byte] => List[TypeMetadata]
  
  /** TODO: will add when adding code to write data out to the github repo */
  val loadMetadata: Array[Byte] => PackageMetadata
  
  val loadTypeInfo: Array[Byte] => TypeDecl
}

object Loader extends TypeStorageLoader {
  override val loadDirectDependencies: (String => Version) => Array[Byte] => List[DirectDependency] = DirectDependencyIonTextLoader.loadFromBytes
  override val loadTransitiveDependencies: (String => Version) => Array[Byte] => List[TransitiveDependency] = TransitiveDependencyIonTextLoader.loadFromBytes
  override val loadMetadata: Array[Byte] => PackageMetadata = bytes => throw new NotImplementedError()
  override val loadMetadataVersions: Array[Byte] => List[String] = MetadataIonTextLoader.loadRawVersionsFromBytes
  override val loadMetadataTypeNames: Array[Byte] => List[TypeMetadata] = bytes => throw new NotImplementedError()
  override val loadTypeInfo: Array[Byte] => TypeDecl = TypeInfoIonTextLoader.loadFromBytes
}
