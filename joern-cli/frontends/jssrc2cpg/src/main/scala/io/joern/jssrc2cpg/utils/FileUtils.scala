package io.joern.jssrc2cpg.utils

import java.nio.file.Files
import java.nio.file.Path
import java.security.DigestInputStream
import java.security.MessageDigest

object FileUtils {

  private def isDirectory(path: Path): Boolean =
    if (path == null || !Files.exists(path)) false
    else Files.isDirectory(path)

  def md5(files: Seq[Path]): String = {
    val md = MessageDigest.getInstance("MD5")
    files
      .filterNot(p => isDirectory(p.toRealPath()))
      .sortBy(_.toRealPath().toString)
      .foreach { path =>
        val dis = new DigestInputStream(Files.newInputStream(path), md)
        while (dis.available() > 0) {
          dis.read()
        }
        dis.close()
      }
    md.digest().map(b => String.format("%02x", Byte.box(b))).mkString
  }

}
