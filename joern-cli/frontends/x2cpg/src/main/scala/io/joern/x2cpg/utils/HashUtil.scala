package io.joern.x2cpg.utils

import java.nio.file.{Files, Path}
import java.security.{DigestInputStream, MessageDigest}
import scala.util.Using

object HashUtil {

  def sha256(file: Path): String =
    sha256(Seq(file))

  def sha256(file: String): String =
    sha256(Seq(Path.of(file)))

  def sha256(files: Seq[Path]): String = {
    val md     = MessageDigest.getInstance("SHA-256")
    val buffer = new Array[Byte](4096)
    files
      .filterNot(p => isDirectory(p.toRealPath()))
      .foreach { path =>
        Using.resource(new DigestInputStream(Files.newInputStream(path), md)) { dis =>
          while (dis.available() > 0) {
            dis.read(buffer)
          }
        }
      }
    md.digest().map(b => String.format("%02x", Byte.box(b))).mkString
  }

  private def isDirectory(path: Path): Boolean =
    if (path == null || !Files.exists(path)) false
    else Files.isDirectory(path)
}
