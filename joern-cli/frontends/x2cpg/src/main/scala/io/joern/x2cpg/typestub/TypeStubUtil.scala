package io.joern.x2cpg.typestub

import java.nio.file.{Path, Paths}

object TypeStubUtil {

  /** Obtains the type stub dir for this frontend.
    * @param metaData
    *   meta data describing the loaded type stubs.
    * @return
    *   the directory where type stubs are.
    */
  def typeStubDir(implicit metaData: TypeStubMetaData): Path = {
    val dir        = metaData.packagePath.toString
    val indexOfLib = dir.lastIndexOf("lib")
    val fixedDir = if (indexOfLib != -1) {
      new java.io.File(dir.substring("file:".length, indexOfLib)).toString
    } else {
      val indexOfTarget = dir.lastIndexOf("target")
      if (indexOfTarget != -1) {
        new java.io.File(dir.substring("file:".length, indexOfTarget)).toString
      } else {
        "."
      }
    }
    Paths.get(fixedDir, "type_stubs")
  }

}
