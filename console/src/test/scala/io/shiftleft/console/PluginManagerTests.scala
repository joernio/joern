package io.shiftleft.console

import better.files.Dsl._
import better.files._
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.attribute.PosixFilePermission

class PluginManagerTests extends AnyWordSpec with Matchers {

  "PluginManager::add" should {
    "not crash if file does not exist" in Fixture() { manager =>
      val testZipFileName = "console/src/test/resources/doesnotexist.zip"
      manager.add(testZipFileName)
    }

    "not crash if file isn't a valid zip" in Fixture() { manager =>
      val testZipFileName = ProjectRoot.relativise("console/src/test/resources/nonzip.zip")
      manager.add(testZipFileName)
    }

    "copy jar files in zip to plugin dir" in Fixture() { manager =>
      val testZipFileName = ProjectRoot.relativise("console/src/test/resources/test.zip")
      manager.add(testZipFileName)
      manager.pluginDir match {
        case Some(dir) =>
          dir.toFile.list().toList shouldBe List("joernext-test-foo.jar")
        case None => fail()
      }
    }

  }

  "PluginManager::rm" should {

    "not crash if name of to-be-removed plugin is incorrect" in Fixture() { manager =>
      manager.rm("somename")
    }

    "remove existing plugin" in Fixture() { manager =>
      val testZipFileName = ProjectRoot.relativise("console/src/test/resources/test.zip")
      manager.add(testZipFileName)
      manager.rm("test").map(x => File(x).name).toSet shouldBe Set("joernext-test-foo.jar")
      manager.listPlugins() shouldBe List()
      manager.add(testZipFileName)
      manager.rm("test").map(x => File(x).name).toSet shouldBe Set("joernext-test-foo.jar")
      manager.listPlugins() shouldBe List()
    }

  }

  "PluginManager::listPlugins" should {

    "display empty plugin list if no plugins exist" in Fixture() { manager =>
      manager.listPlugins() shouldBe List()
    }

    "display plugin after adding it" in Fixture() { manager =>
      val testZipFileName = ProjectRoot.relativise("console/src/test/resources/test.zip")
      manager.add(testZipFileName)
      manager.listPlugins() shouldBe List("test")
    }
  }

}

object Fixture {

  def apply[T]()(f: PluginManager => T): T = {
    val dir = File.newTemporaryDirectory("pluginmantests")
    mkdir(dir / "lib")
    mkdirs(dir / "schema-extender" / "schemas")
    val extender = dir / "schema-extender.sh"
    val extenderContents = "#!/bin/sh\necho 'foo' > " + (dir / "out.txt")
    extender.write(extenderContents)
    chmod_+(PosixFilePermission.OWNER_EXECUTE, extender)
    val result = f(new PluginManager(dir))
    dir.delete()
    result
  }
}
