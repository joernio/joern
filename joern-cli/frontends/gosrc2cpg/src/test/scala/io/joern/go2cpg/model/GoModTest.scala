package io.joern.go2cpg.model;

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.model.{GoMod, GoModDependency, GoModModule}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class GoModTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  "go project without .mod file" in {
    val namespace = GoMod.getNameSpace(Paths.get("").toAbsolutePath.toString, "main")
    namespace shouldBe "main"
  }
  "invalid compilation file unit with main pkg" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    var namespace =
      GoMod.getNameSpace(null, "main")
    namespace shouldBe "main"
    namespace = GoMod.getNameSpace("", "main")
    namespace shouldBe "main"

  }
  "with .mod file and main pkg 1 use case" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "first", "second", "test.go").toAbsolutePath.toString, "main")
    namespace shouldBe "first/second/main"
  }

  "with .mod file and main pkg 2 use case" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString + "/"
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "first", "second", "test.go").toAbsolutePath.toString, "main")
    namespace shouldBe "first/second/main"
  }

  "with .mod file and main pkg 3 use case" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "test.go").toAbsolutePath.toString, "main")
    namespace shouldBe "main"
  }

  "with .mod file and pkg other than main matching with folder" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "test.go").toAbsolutePath.toString, "trial")
    namespace shouldBe "joern.io/trial"
  }

  "with .mod file, pkg other than main, one level child folder, and package matching with last folder" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "first", "test.go").toAbsolutePath.toString, "first")
    namespace shouldBe "joern.io/trial/first"
  }

  "with .mod file and pkg other than main and not matching with folder" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "test.go").toAbsolutePath.toString, "foo")
    namespace shouldBe "joern.io/trial>foo"
  }

  "with .mod file, pkg other than main, one level child folder, and package not matching with last folder" in {
    val config = Config()
    config.inputPath = Paths.get("").toAbsolutePath.toString
    GoMod.config = config
    GoMod.meta = GoMod(
      fileFullPath = Paths.get(config.inputPath, "go.mod").toAbsolutePath.toString,
      module = GoModModule("joern.io/trial"),
      dependencies = List[GoModDependency]()
    )
    val namespace =
      GoMod.getNameSpace(Paths.get(config.inputPath, "first", "test.go").toAbsolutePath.toString, "bar")
    namespace shouldBe "joern.io/trial/first>bar"
  }
}
