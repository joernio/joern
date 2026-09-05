package io.shiftleft.semanticcpg.utils

import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class FileUtilTests extends AnyWordSpec with Matchers {

  private def writeZip(zip: Path, entries: Seq[(String, String)]): Unit = {
    Using.resource(new ZipOutputStream(Files.newOutputStream(zip))) { zos =>
      entries.foreach { case (name, content) =>
        zos.putNextEntry(new ZipEntry(name))
        zos.write(content.getBytes(StandardCharsets.UTF_8))
        zos.closeEntry()
      }
    }
  }

  "FileUtil.unzipTo" should {

    // Guards against per-entry resource accumulation silently truncating the output: every
    // entry must be extracted. (Reliably reproducing the truncation itself requires exceeding
    // an environment-specific resource limit, so this is a functional invariant check.)
    "extract every entry of an archive with many entries" in {
      FileUtil.usingTemporaryDirectory("unzip-many") { workDir =>
        val n   = 5000
        val zip = workDir.resolve("many.zip")
        writeZip(zip, (0 until n).map(i => s"pkg/cls$i.txt" -> i.toString))

        val dest = Files.createDirectory(workDir.resolve("out"))
        zip.unzipTo(dest)

        val extracted =
          Using.resource(Files.walk(dest))(_.iterator().asScala.count(Files.isRegularFile(_)))
        extracted shouldBe n
      }
    }

    // Guards against the failure being swallowed: with the previous implementation the
    // Using.Manager result Try was discarded and `destination` was returned unconditionally,
    // so a mid-extraction failure was reported as a successful (partial) extraction. It must
    // now propagate. Here "a" is extracted as a regular file, then "a/b" needs "a" to be a
    // directory, which fails part-way through.
    "surface a mid-extraction failure instead of returning a partial extraction as success" in {
      FileUtil.usingTemporaryDirectory("unzip-fail") { workDir =>
        val zip = workDir.resolve("collide.zip")
        writeZip(zip, Seq("a" -> "x", "a/b" -> "y"))

        val dest = Files.createDirectory(workDir.resolve("out"))
        a[Throwable] should be thrownBy zip.unzipTo(dest)
      }
    }
  }
}
