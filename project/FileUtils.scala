import org.apache.commons.compress.archivers.tar._
import org.apache.commons.compress.utils.IOUtils

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Files
import scala.collection.JavaConverters._

object FileUtils {

  def listFilesRecursively(roots: File*): Seq[File] = {
    roots.flatMap { root =>
      Files.walk(root.toPath).iterator.asScala.map(_.toFile).filter(!_.isDirectory)
    }
  }

  def deleteRecursively(root: File): Unit = {
    if (root.exists) {
      Files.walk(root.toPath).iterator.asScala.map(_.toFile).collect {
        case file if (file.isDirectory) => deleteRecursively(file)
        case file => file.delete()
      }
    }
  }

  def createTar(outputFile: File, inputRoots: File*): Unit = {
    if (outputFile.exists) outputFile.delete()

    val tarOutputStream = new TarArchiveOutputStream(new FileOutputStream(outputFile))
    inputRoots.flatMap { root =>
      Files.walk(root.toPath).iterator.asScala.map { path =>
        val file = path.toFile
        tarOutputStream.putArchiveEntry(tarOutputStream.createArchiveEntry(file, file.getPath))
        if (file.isFile) {
          val fileInputStream = new FileInputStream(file)
          IOUtils.copy(fileInputStream, tarOutputStream)
          fileInputStream.close()
        }
        tarOutputStream.closeArchiveEntry()
      }
    }
    tarOutputStream.finish()
  }
}
