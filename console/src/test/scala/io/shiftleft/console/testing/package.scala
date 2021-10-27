package io.shiftleft.console
import better.files.Dsl._
import better.files._
import io.shiftleft.console.workspacehandling.Project

import scala.util.{Failure, Success, Try}

package object testing {

  def createStandaloneCpg(console: Console[Project], codeDir: File): File = {
    val tmpProjectName = "standalonecpg"
    console.importCode(codeDir.toString, tmpProjectName)
    val project = console.workspace.project(tmpProjectName)
    val cpgPath = project.get.path.resolve("cpg.bin")
    val tmpCpg = File.newTemporaryFile("console")
    Try {
      cp(cpgPath, tmpCpg)
      console.workspace.reset()
      tmpCpg
    } match {
      case Success(v) => v
      case Failure(exc) =>
        tmpCpg.delete()
        throw exc
    }
  }

}
