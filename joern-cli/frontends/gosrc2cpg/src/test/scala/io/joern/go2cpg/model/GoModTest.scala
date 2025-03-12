package io.joern.go2cpg.model;

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.model.{GoMod, GoModDependency, GoModHelper, GoModModule}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.io.File as JFile
import java.nio.file.{Files, Path, Paths}
import scala.language.postfixOps

class GoModTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  "go project without .mod file" in {
    val goMod     = new GoModHelper()
    val namespace = goMod.getNameSpace(FileUtil.currentWorkingDirectory.toString, "main")
    namespace shouldBe "main"
  }
  "invalid compilation file unit with main pkg" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    var namespace =
      goMod.getNameSpace(null, "main")
    namespace shouldBe "main"
    namespace = goMod.getNameSpace("", "main")
    namespace shouldBe "main"

  }
  "with .mod file and main pkg 1 use case" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "first" / "second" / "test.go").toString, "main")
    namespace shouldBe "first/second/main"
  }

  "with .mod file and main pkg 2 use case" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "first" / "second" / "test.go").toString, "main")
    namespace shouldBe "first/second/main"
  }

  "with .mod file and main pkg 3 use case" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "test.go").toString, "main")
    namespace shouldBe "main"
  }

  "with .mod file and pkg other than main matching with folder" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "test.go").toString, "trial")
    namespace shouldBe "joern.io/trial"
  }

  "with .mod file, pkg other than main, one level child folder, and package matching with last folder" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "first" / "test.go").toString, "first")
    namespace shouldBe "joern.io/trial/first"
  }

  "with .mod file and pkg other than main and not matching with folder" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "test.go").toString, "foo")
    namespace shouldBe "joern.io/trial"
  }

  "with .mod file, pkg other than main, one level child folder, and package not matching with last folder" in {
    val inputPath = FileUtil.currentWorkingDirectory.toString + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = (Paths.get(inputPath) / "go.mod").toString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace((Paths.get(inputPath) / "first" / "test.go").toString, "bar")
    namespace shouldBe "joern.io/trial/first"
  }
}
