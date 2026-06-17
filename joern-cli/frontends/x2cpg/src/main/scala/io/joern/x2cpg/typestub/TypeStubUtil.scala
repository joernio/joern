package io.joern.x2cpg.typestub

import java.nio.file.{Path, Paths}

object TypeStubUtil {

  /** Obtains the type stub dir for this frontend.
    * @return
    *   the directory where type stubs are.
    */
  def typeStubDir(codeSourceLocation: String): Path = {
    val indexOfLib = codeSourceLocation.lastIndexOf("lib")
    val fixedDir = if (indexOfLib != -1) {
      new java.io.File(codeSourceLocation.substring("file:".length, indexOfLib)).toString
    } else {
      val indexOfTarget = codeSourceLocation.lastIndexOf("target")
      if (indexOfTarget != -1) {
        new java.io.File(codeSourceLocation.substring("file:".length, indexOfTarget)).toString
      } else {
        "."
      }
    }
    Paths.get(fixedDir, "type_stubs")
  }

}
