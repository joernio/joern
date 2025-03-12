package io.shiftleft.resolver.util

import io.shiftleft.resolver.api.Id

import java.nio.file.Path

object GitStorage {
  def calcMetaDataPath(basePath: Path, idValue: String, version: String): Path = {
    val escId = escapedIdValue(idValue)
    val escVersion = escapedVersion(version)
    basePath.resolve(escId).resolve(escVersion).resolve("metadata").resolve("metadata.ion")
  }
  
  def calcLibInfoPath(basePath: Path, idValue: String, version: String): Path = {
    val escId = escapedIdValue(idValue)
    val escVersion = escapedVersion(version)
    basePath.resolve(escId).resolve(escVersion).resolve("libinfo.ion")
  }
  
  private def escapedIdValue(idValue: String): String = {
    // TODO extend this as some characters are not allowed on certain file systems
    idValue.replace(':', '-')
  }

  private def escapedVersion(version: String): String = {
    // TODO extend this as some characters are not allowed on certain file systems
    version
  }

}
