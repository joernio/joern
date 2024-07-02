import java.io.File
import java.nio.file.Files
import scala.collection.JavaConverters.*

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
        case file                       => file.delete()
      }
    }
  }

}
