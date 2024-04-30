package io.joern.x2cpg.typestub

import better.files.File

import java.nio.file.Paths

object TypeStubUtil {

  /** Obtains the type stub dir for this frontend.
    * @param metaData
    *   meta data describing the loaded type stubs.
    * @return
    *   the directory where type stubs are.
    */
  def typeStubDir(implicit metaData: TypeStubMetaData): File = {
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
    File(Paths.get(fixedDir, "/type_stubs").toAbsolutePath)
  }

}
