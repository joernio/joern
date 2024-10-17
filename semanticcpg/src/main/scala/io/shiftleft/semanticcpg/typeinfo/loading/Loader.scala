package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.{TypeDecl, TypeInfoIonTextLoader, TypeMetadata}
import io.shiftleft.semanticcpg.typeinfo.dependencies.{DirectDependency, TransitiveDependency}
import io.shiftleft.semanticcpg.typeinfo.version.Version

trait TypeStorageLoader {
  /** Takes a version parser and an array of bytes to parse */
  val loadDirectDependencies: (String => Version) => Array[Byte] => List[DirectDependency]

  /** Takes a version parser and an array of bytes to parse */
  val loadTransitiveDependencies: (String => Version) => Array[Byte] => List[TransitiveDependency]
  
  /** Loads raw version strings, to be parsed by a deriver of Version */
  val loadMetadataVersions: Array[Byte] => List[String]
  
  val loadMetadataTypeNames: Array[Byte] => List[TypeMetadata]
  
  val loadTypeInfo: Array[Byte] => TypeDecl
}

object Loader extends TypeStorageLoader {
  override val loadDirectDependencies: (String => Version) => Array[Byte] => List[DirectDependency] = DirectDependencyIonTextLoader.loadFromBytes
  override val loadTransitiveDependencies: (String => Version) => Array[Byte] => List[TransitiveDependency] = TransitiveDependencyIonTextLoader.loadFromBytes
  override val loadMetadataVersions: Array[Byte] => List[String] = ???
  override val loadMetadataTypeNames: Array[Byte] => List[TypeMetadata] = ???
  override val loadTypeInfo: Array[Byte] => TypeDecl = TypeInfoIonTextLoader.loadFromBytes
}
