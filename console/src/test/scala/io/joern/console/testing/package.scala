package io.joern.console

import io.joern.console.workspacehandling.Project
import flatgraph.help.Table.{AvailableWidthProvider, ConstantWidth}
import io.joern.x2cpg.utils.FileUtil

import java.nio.file.Path
import scala.util.Try

package object testing {

  implicit val availableWidthProvider: AvailableWidthProvider = ConstantWidth(120)

  object WithStandaloneCpg {
    def apply(console: Console[Project], codeDir: Path)(fun: Path => Unit): Unit = {
      val tmpProjectName = "standalonecpg"
      console.importCode(codeDir.toString, tmpProjectName)
      val project = console.workspace.project(tmpProjectName)
      val cpgPath = project.get.path.resolve("cpg.bin")
      FileUtil.usingTemporaryFile("console", suffix = "cpg.bin") { tmpCpg =>
        FileUtil.copyFiles(cpgPath, tmpCpg)
        Try(console.workspace.reset)
        fun(tmpCpg)
      }
    }
  }

}
