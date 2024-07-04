package io.joern.go2cpg.model;

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.model.{GoMod, GoModDependency, GoModHelper, GoModModule}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File as JFile
import scala.language.postfixOps

class GoModTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  "go project without .mod file" in {
    val goMod     = new GoModHelper()
    val namespace = goMod.getNameSpace(File.currentWorkingDirectory.toString(), "main")
    namespace shouldBe "main"
  }
  "invalid compilation file unit with main pkg" in {
    val inputPath = File.currentWorkingDirectory.toString()
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
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
    val inputPath = File.currentWorkingDirectory.toString()
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "first" / "second" / "test.go" pathAsString, "main")
    namespace shouldBe "first/second/main"
  }

  "with .mod file and main pkg 2 use case" in {
    val inputPath = File.currentWorkingDirectory.toString() + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "first" / "second" / "test.go" pathAsString, "main")
    namespace shouldBe "first/second/main"
  }

  "with .mod file and main pkg 3 use case" in {
    val inputPath = File.currentWorkingDirectory.toString() + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "test.go" pathAsString, "main")
    namespace shouldBe "main"
  }

  "with .mod file and pkg other than main matching with folder" in {
    val inputPath = File.currentWorkingDirectory.toString() + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "test.go" pathAsString, "trial")
    namespace shouldBe "joern.io/trial"
  }

  "with .mod file, pkg other than main, one level child folder, and package matching with last folder" in {
    val inputPath = File.currentWorkingDirectory.toString() + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "first" / "test.go" pathAsString, "first")
    namespace shouldBe "joern.io/trial/first"
  }

  "with .mod file and pkg other than main and not matching with folder" in {
    val inputPath = File.currentWorkingDirectory.toString() + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "test.go" pathAsString, "foo")
    namespace shouldBe "joern.io/trial"
  }

  "with .mod file, pkg other than main, one level child folder, and package not matching with last folder" in {
    val inputPath = File.currentWorkingDirectory.toString() + JFile.separator
    val goMod = new GoModHelper(
      Some(inputPath),
      Some(
        GoMod(
          fileFullPath = File(inputPath) / "go.mod" pathAsString,
          module = GoModModule("joern.io/trial"),
          dependencies = List[GoModDependency]()
        )
      )
    )
    val namespace =
      goMod.getNameSpace(File(inputPath) / "first" / "test.go" pathAsString, "bar")
    namespace shouldBe "joern.io/trial/first"
  }
}
