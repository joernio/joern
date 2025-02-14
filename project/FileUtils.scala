import java.io.File
import java.nio.file.{Files, FileSystems}
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
        case file if file.isDirectory => deleteRecursively(file)
        case file                     => file.delete()
      }
    }
  }

  /** checks if the given jar contains the given entry */
  def jarContainsEntry(jar: File, entry: String, onlyAtRootLevel: Boolean = false): Boolean = {
    val zipFs = FileSystems.newFileSystem(jar.toPath)
    val result = zipFs.getRootDirectories.asScala.exists { zipRootDir =>
      val iterable =
        if (onlyAtRootLevel) Files.list(zipRootDir)
        else Files.walk(zipRootDir)
      iterable.iterator.asScala.exists(_.toString == "/module-info.class")
    }
    zipFs.close()
    result
  }

  /** removes the given entry from the given jar */
  def removeJarEntry(jar: File, entry: String, onlyAtRootLevel: Boolean = false): Unit = {
    val zipFs = FileSystems.newFileSystem(jar.toPath)
    zipFs.getRootDirectories.forEach { zipRootDir =>
      val iterable =
        if (onlyAtRootLevel) Files.list(zipRootDir)
        else Files.walk(zipRootDir)
      iterable.filter(_.toString == entry).forEach(Files.delete(_))
    }
    zipFs.close()
  }


}
